#!/usr/bin/env bash
# Test script for wisp self-hosting compiler components
set -e

cd "$(dirname "$0")/.."

echo "=========================================="
echo "Testing Wisp Self-Hosting Compiler"
echo "=========================================="
echo ""

# Helper to run wisp
run() {
  cargo run --quiet -- "$@" 2>/dev/null
}

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

pass() { echo -e "${GREEN}✓ PASS${NC}: $1"; }
fail() { echo -e "${RED}✗ FAIL${NC}: $1"; exit 1; }

# 1. Test LEB128 encoding
echo "1. Testing LEB128 encoding (leb128.wisp)"
echo "   ----------------------------------------"
run compile examples/leb128.wisp examples/leb128

# Test byte counts
R=$(run run examples/leb128.wasm test-uleb128-zero)
[ "$R" = "1" ] && pass "uleb128(0) = 1 byte" || fail "uleb128(0) expected 1, got $R"

R=$(run run examples/leb128.wasm test-uleb128-max-single)
[ "$R" = "1" ] && pass "uleb128(127) = 1 byte" || fail "uleb128(127) expected 1, got $R"

R=$(run run examples/leb128.wasm test-uleb128-two-bytes)
[ "$R" = "2" ] && pass "uleb128(128) = 2 bytes" || fail "uleb128(128) expected 2, got $R"

R=$(run run examples/leb128.wasm test-uleb128-three-bytes)
[ "$R" = "3" ] && pass "uleb128(624485) = 3 bytes" || fail "uleb128(624485) expected 3, got $R"

# Test actual byte values for 128 -> [0x80, 0x01]
B0=$(run run examples/leb128.wasm test-encode-byte 128 0)
B1=$(run run examples/leb128.wasm test-encode-byte 128 1)
[ "$B0" = "128" ] && [ "$B1" = "1" ] && pass "uleb128(128) = [0x80, 0x01]" || fail "uleb128(128) bytes wrong"

echo ""

# 2. Test WASM module emitter
echo "2. Testing WASM module emitter (wasm_codegen.wisp)"
echo "   ------------------------------------------------"
run compile examples/wasm_codegen.wisp examples/wasm_codegen

SIZE=$(run run examples/wasm_codegen.wasm emit-add)
[ "$SIZE" = "41" ] && pass "emit-add produces 41 bytes" || fail "emit-add expected 41, got $SIZE"

MAGIC=$(run run examples/wasm_codegen.wasm get-magic)
[ "$MAGIC" = "1836278016" ] && pass "Magic = 0x6D736100 (\\0asm)" || fail "Wrong magic: $MAGIC"

echo ""

# 3. Test factorial emitter
echo "3. Testing factorial emitter (wasm_factorial.wisp)"
echo "   ------------------------------------------------"
run compile examples/wasm_factorial.wisp examples/wasm_factorial

SIZE=$(run run examples/wasm_factorial.wasm emit-factorial)
[ "$SIZE" = "62" ] && pass "emit-factorial produces 62 bytes" || fail "emit-factorial expected 62, got $SIZE"

# Extract and run the generated factorial module
BYTES=""
for i in $(seq 0 61); do
  B=$(run run examples/wasm_factorial.wasm get-byte $i)
  BYTES="$BYTES$(printf "%02X" $B)"
done
echo "$BYTES" | xxd -r -p > /tmp/factorial_test.wasm

# Create Node.js test
cat > /tmp/test_factorial.js << 'EOF'
const fs = require("fs");
const wasm = fs.readFileSync("/tmp/factorial_test.wasm");
const mod = new WebAssembly.Module(wasm);
const instance = new WebAssembly.Instance(mod);
const f = instance.exports.factorial;
const results = [f(0), f(1), f(5), f(10)];
console.log(results.join(","));
EOF

RESULT=$(nix-shell -p nodejs --run "node /tmp/test_factorial.js" 2>/dev/null)
[ "$RESULT" = "1,1,120,3628800" ] && pass "Generated factorial computes correctly!" || fail "Factorial results: $RESULT"

echo ""

# 4. Test tokenizer
echo "4. Testing tokenizer (tokenizer.wisp)"
echo "   -----------------------------------"
run compile examples/tokenizer.wisp examples/tokenizer

COUNT=$(run run examples/tokenizer.wasm test-simple)
[ "$COUNT" = "5" ] && pass "Tokenize '(add 1 2)' = 5 tokens" || fail "Expected 5 tokens, got $COUNT"

# Check token types: LPAREN, SYMBOL, NUMBER, NUMBER, RPAREN
T0=$(run run examples/tokenizer.wasm get-token-type 0)
T1=$(run run examples/tokenizer.wasm get-token-type 1)
T2=$(run run examples/tokenizer.wasm get-token-type 2)
T3=$(run run examples/tokenizer.wasm get-token-type 3)
T4=$(run run examples/tokenizer.wasm get-token-type 4)

[ "$T0" = "0" ] && [ "$T1" = "2" ] && [ "$T2" = "3" ] && [ "$T3" = "3" ] && [ "$T4" = "1" ] && \
  pass "Token types: LPAREN SYMBOL NUMBER NUMBER RPAREN" || \
  fail "Wrong token types: $T0 $T1 $T2 $T3 $T4"

echo ""

# 5. Test parser
echo "5. Testing parser (parser.wisp)"
echo "   -----------------------------"
run compile examples/parser.wisp examples/parser

SIZE=$(run run examples/parser.wasm test-parse)
[ "$SIZE" = "21" ] && pass "Parse '(add 1 2)' = 21 byte AST" || fail "Expected 21 bytes, got $SIZE"

# Check AST structure: LIST with 3 children
NODE_TYPE=$(run run examples/parser.wasm get-ast-byte 0)
CHILD_COUNT=$(run run examples/parser.wasm get-ast-i32 1)

[ "$NODE_TYPE" = "1" ] && [ "$CHILD_COUNT" = "3" ] && \
  pass "AST: LIST with 3 children" || \
  fail "Wrong AST structure: type=$NODE_TYPE count=$CHILD_COUNT"

echo ""

# 6. Test code generator (full pipeline!)
echo "6. Testing code generator (codegen.wisp)"
echo "   --------------------------------------"
run compile examples/codegen.wisp examples/codegen

SIZE=$(run run examples/codegen.wasm test-compile)
[ "$SIZE" = "5" ] && pass "Compile '(i32.add 1 2)' = 5 bytes" || fail "Expected 5 bytes, got $SIZE"

# Check generated bytes: 41 01 41 02 6A
B0=$(run run examples/codegen.wasm get-code-byte 0)
B1=$(run run examples/codegen.wasm get-code-byte 1)
B2=$(run run examples/codegen.wasm get-code-byte 2)
B3=$(run run examples/codegen.wasm get-code-byte 3)
B4=$(run run examples/codegen.wasm get-code-byte 4)

[ "$B0" = "65" ] && [ "$B1" = "1" ] && [ "$B2" = "65" ] && [ "$B3" = "2" ] && [ "$B4" = "106" ] && \
  pass "Generated: i32.const 1, i32.const 2, i32.add" || \
  fail "Wrong bytecode: $B0 $B1 $B2 $B3 $B4"

echo ""
echo "=========================================="
echo -e "${GREEN}All tests passed!${NC}"
echo "=========================================="
echo ""
echo "The wisp self-hosting compiler pipeline works:"
echo "  Source -> Tokenizer -> Parser -> CodeGen -> WASM"
