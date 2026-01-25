# String Operations

**Status**: Complete
**Completed**: 2026-01-25
**Related Proposal**: [Self-Hosted Compiler Plan](SELF-HOSTED-COMPILER.md)

## Objective

Add string manipulation operations to Wisp, enabling the implementation of a self-hosted compiler. A compiler needs to tokenize, parse, and transform source code - all of which require string operations.

## Motivation

Before this change, Wisp had limited string support:
- String literals could be created: `"hello"`
- String length could be queried: `(string-len s)`
- Strings could be passed to/from functions

But there was no way to:
- Access individual characters
- Extract substrings
- Concatenate strings
- Compare strings

These operations are essential for writing a tokenizer and parser in Wisp itself.

## Operations Added

### `(string-len s)` → s32
Returns the length of a string in bytes.

```lisp
(string-len "hello")  ; → 5
(string-len "")       ; → 0
```

### `(string-ref s idx)` → s32
Returns the byte value at the given index (0-based).

```lisp
(let (s "hello")
  (string-ref s (i32.const 0)))  ; → 104 ('h')

(let (s "hello")
  (string-ref s (i32.const 4)))  ; → 111 ('o')
```

**Note**: Returns raw byte value, not a character type. For ASCII, byte value equals character code.

### `(substring s start end)` → string
Extracts a portion of a string from `start` (inclusive) to `end` (exclusive).

```lisp
(let (s "hello world")
  (substring s (i32.const 0) (i32.const 5)))   ; → "hello"

(let (s "hello world")
  (substring s (i32.const 6) (i32.const 11)))  ; → "world"
```

### `(string-append s1 s2)` → string
Concatenates two strings, returning a new string.

```lisp
(let (s1 "hello")
  (let (s2 " world")
    (string-append s1 s2)))  ; → "hello world"
```

### `(string=? s1 s2)` → s32
Compares two strings for equality. Returns 1 if equal, 0 if not.

```lisp
(string=? "hello" "hello")  ; → 1 (true)
(string=? "hello" "world")  ; → 0 (false)
(string=? "hello" "hi")     ; → 0 (false, different lengths)
(string=? "" "")            ; → 1 (true)
```

## Memory Layout

Strings in Wisp use a simple memory layout:

```
┌─────────────┬─────────────────────────┐
│ length (4B) │ UTF-8 bytes (N bytes)   │
└─────────────┴─────────────────────────┘
     i32           byte array
```

- First 4 bytes: 32-bit length (number of bytes, not characters)
- Remaining bytes: UTF-8 encoded string data

String pointers point to the start of this structure (the length field).

## Implementation Details

### Code Generation

Each operation generates inline WebAssembly:

**string-ref**: Calculates address `(string_ptr + 4 + index)` and loads a byte.

**substring**:
1. Allocates new string on heap
2. Calculates new length (`end - start`)
3. Uses `memory.copy` to copy byte range
4. Returns pointer to new string

**string-append**:
1. Gets lengths of both strings
2. Allocates space for combined length + 4
3. Copies first string's bytes
4. Copies second string's bytes after first
5. Returns pointer to new string

**string=?**:
1. Compares lengths (fast path: different lengths → not equal)
2. If lengths match, compares bytes in a loop
3. Returns 1 if all bytes match, 0 otherwise

### Type Checking

All string operations enforce type constraints:
- String arguments must be `Type::Str`
- Index arguments must be `Type::S32`
- Return types are `Type::Str` or `Type::S32` as appropriate

### Heap Usage

Operations that create new strings (`substring`, `string-append`) allocate from the heap pointer `$__heap_ptr`. The heap grows upward from its initial value (typically 49152 / 0xC000).

## Examples

### Building a Simple Tokenizer Character Check

```lisp
; Check if character is whitespace
(fn is-whitespace ((c s32)) s32
  (if (i32.eq c (i32.const 32))    ; space
    (i32.const 1)
    (if (i32.eq c (i32.const 10))  ; newline
      (i32.const 1)
      (if (i32.eq c (i32.const 9)) ; tab
        (i32.const 1)
        (i32.const 0)))))

; Get first non-whitespace position
(fn skip-whitespace ((s string) (pos s32)) s32
  (let (len (string-len s))
    (if (i32.ge_s pos len)
      pos
      (let (c (string-ref s pos))
        (if (is-whitespace c)
          (skip-whitespace s (i32.add pos (i32.const 1)))
          pos)))))
```

### String Comparison for Keywords

```lisp
; Check if a token is a keyword
(fn is-keyword ((token string)) s32
  (if (string=? token "fn")
    (i32.const 1)
    (if (string=? token "let")
      (i32.const 1)
      (if (string=? token "if")
        (i32.const 1)
        (i32.const 0)))))
```

## Testing

Tests are in `tests/string_ops.rs`:

```
running 17 tests
test test_empty_string_len ... ok
test test_string_append_boundary ... ok
test test_string_append_first_char ... ok
test test_string_append_last_char ... ok
test test_string_append_len ... ok
test test_string_eq_different ... ok
test test_string_eq_different_len ... ok
test test_string_eq_empty ... ok
test test_string_eq_one_empty ... ok
test test_string_eq_same ... ok
test test_string_len ... ok
test test_string_ref_first ... ok
test test_string_ref_last ... ok
test test_string_ref_middle ... ok
test test_substring_content ... ok
test test_substring_len ... ok
test test_substring_middle ... ok

test result: ok. 17 passed
```

## Future Enhancements

Potential additions for future phases:
- `(string-find s needle)` → index or -1
- `(char->string c)` → single-character string
- `(string->list s)` → list of byte values
- `(list->string bytes)` → string from byte list
- Unicode-aware operations (character vs byte indexing)

## Related Files

- `src/compiler.rs` - Implementation of string operations
- `tests/string_ops.rs` - Test suite
- `examples/string-test.lisp` - Example usage
