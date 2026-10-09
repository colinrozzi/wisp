use super::*;

// Lexer: source text -> tokens. The token types and the numeric-literal
// helpers live here with `tokenize`.
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    LParen,
    RParen,
    Symbol(String),
    Number(NumericToken),
    String(String), // String literal
    Quasiquote,     // `
    Unquote,        // ,
    UnquoteSplice,  // ,@
    SyntaxQuote,    // #'
    Quasisyntax,    // #`
    Unsyntax,       // #,
    UnsyntaxSplice, // #,@
}

#[derive(Debug, Clone)]
pub enum NumericToken {
    Int { value: i64, ty: Type },
    Float { value: f64, ty: Type },
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut line = 1usize;
    let mut column = 1usize;

    while let Some(&ch) = chars.peek() {
        match ch {
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    span: Span::new(line, column, 1),
                });
                chars.next();
                column += 1;
            }
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    span: Span::new(line, column, 1),
                });
                chars.next();
                column += 1;
            }
            '`' => {
                tokens.push(Token {
                    kind: TokenKind::Quasiquote,
                    span: Span::new(line, column, 1),
                });
                chars.next();
                column += 1;
            }
            ',' => {
                let start_col = column;
                chars.next();
                column += 1;
                if chars.peek() == Some(&'@') {
                    chars.next();
                    column += 1;
                    tokens.push(Token {
                        kind: TokenKind::UnquoteSplice,
                        span: Span::new(line, start_col, 2),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Unquote,
                        span: Span::new(line, start_col, 1),
                    });
                }
            }
            '#' => {
                let start_col = column;
                chars.next();
                column += 1;
                match chars.peek() {
                    Some(&'\'') => {
                        chars.next();
                        column += 1;
                        tokens.push(Token {
                            kind: TokenKind::SyntaxQuote,
                            span: Span::new(line, start_col, 2),
                        });
                    }
                    Some(&'`') => {
                        chars.next();
                        column += 1;
                        tokens.push(Token {
                            kind: TokenKind::Quasisyntax,
                            span: Span::new(line, start_col, 2),
                        });
                    }
                    Some(&',') => {
                        chars.next();
                        column += 1;
                        if chars.peek() == Some(&'@') {
                            chars.next();
                            column += 1;
                            tokens.push(Token {
                                kind: TokenKind::UnsyntaxSplice,
                                span: Span::new(line, start_col, 3),
                            });
                        } else {
                            tokens.push(Token {
                                kind: TokenKind::Unsyntax,
                                span: Span::new(line, start_col, 2),
                            });
                        }
                    }
                    _ => {
                        // Treat # as start of a symbol (e.g., #t, #f)
                        let mut lexeme = String::from("#");
                        while let Some(&c2) = chars.peek() {
                            if c2.is_whitespace()
                                || c2 == '('
                                || c2 == ')'
                                || c2 == '`'
                                || c2 == ','
                                || c2 == ';'
                                || c2 == '\''
                            {
                                break;
                            }
                            lexeme.push(c2);
                            chars.next();
                            column += 1;
                        }
                        tokens.push(Token {
                            kind: TokenKind::Symbol(lexeme),
                            span: Span::new(line, start_col, column - start_col),
                        });
                    }
                }
            }
            ';' => {
                // Skip comments (everything until end of line)
                while let Some(&c) = chars.peek() {
                    chars.next();
                    if c == '\n' {
                        line += 1;
                        column = 1;
                        break;
                    } else {
                        column += 1;
                    }
                }
            }
            '"' => {
                // String literal
                let start_col = column;
                chars.next(); // consume opening quote
                column += 1;
                let mut content = String::new();
                while let Some(&c) = chars.peek() {
                    chars.next();
                    column += 1;
                    if c == '"' {
                        break;
                    } else if c == '\\' {
                        // Handle escape sequences
                        if let Some(&escaped) = chars.peek() {
                            chars.next();
                            column += 1;
                            match escaped {
                                'n' => content.push('\n'),
                                't' => content.push('\t'),
                                'r' => content.push('\r'),
                                '"' => content.push('"'),
                                '\\' => content.push('\\'),
                                'x' => {
                                    // \xHH hex escape
                                    let mut hex = String::new();
                                    for _ in 0..2 {
                                        if let Some(&h) = chars.peek()
                                            && h.is_ascii_hexdigit()
                                        {
                                            hex.push(h);
                                            chars.next();
                                            column += 1;
                                        }
                                    }
                                    if hex.len() == 2 {
                                        let byte = u8::from_str_radix(&hex, 16).unwrap();
                                        content.push(byte as char);
                                    } else {
                                        content.push('\\');
                                        content.push('x');
                                        content.push_str(&hex);
                                    }
                                }
                                _ => {
                                    content.push('\\');
                                    content.push(escaped);
                                }
                            }
                        }
                    } else if c == '\n' {
                        content.push(c);
                        line += 1;
                        column = 1;
                    } else {
                        content.push(c);
                    }
                }
                tokens.push(Token {
                    kind: TokenKind::String(content),
                    span: Span::new(line, start_col, column - start_col),
                });
            }
            '\n' => {
                chars.next();
                line += 1;
                column = 1;
            }
            _ => {
                if ch.is_whitespace() {
                    chars.next();
                    column += 1;
                    continue;
                }
                let start_col = column;
                let mut lexeme = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2.is_whitespace()
                        || c2 == '('
                        || c2 == ')'
                        || c2 == '`'
                        || c2 == ','
                        || c2 == ';'
                    {
                        break;
                    }
                    lexeme.push(c2);
                    chars.next();
                    column += 1;
                }
                let span = Span::new(line, start_col, lexeme.len());
                if let Some(num) = parse_numeric_token(&lexeme) {
                    tokens.push(Token {
                        kind: TokenKind::Number(num),
                        span,
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Symbol(lexeme),
                        span,
                    });
                }
            }
        }
    }

    tokens
}

fn parse_numeric_token(raw: &str) -> Option<NumericToken> {
    let (base, explicit_type) = strip_numeric_suffix(raw)?;

    let is_float = base.contains('.') || matches!(explicit_type, Some(Type::F32 | Type::F64));
    if is_float {
        let value: f64 = base.parse().ok()?;
        let ty = explicit_type.unwrap_or(Type::F64);
        match ty {
            Type::F32 | Type::F64 => Some(NumericToken::Float { value, ty }),
            _ => None,
        }
    } else {
        let value: i64 = base.parse().ok()?;
        let ty = explicit_type.unwrap_or(Type::S32);
        match ty {
            Type::S32 | Type::S64 => Some(NumericToken::Int { value, ty }),
            _ => None,
        }
    }
}

fn strip_numeric_suffix(raw: &str) -> Option<(&str, Option<Type>)> {
    if raw.is_empty() {
        return None;
    }
    let suffixes = [("s64", Type::S64), ("f32", Type::F32), ("f64", Type::F64)];
    for (suffix, ty) in suffixes {
        if let Some(base) = raw.strip_suffix(suffix) {
            return Some((base, Some(ty)));
        }
    }
    Some((raw, None))
}
