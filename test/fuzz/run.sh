#!/usr/bin/env bash
# Build test/fuzz/lexer_fuzz.c with libFuzzer, AddressSanitizer and UndefinedBehaviorSanitizer, then run it.
# usage: test/fuzz/run.sh [corpus or seed directories] [libFuzzer options]
# CC picks the compiler (default clang). FUZZ_CFLAGS adds flags. FUZZ_OUT holds the binary and corpus.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out=${FUZZ_OUT:-$root/fuzz-out}
mkdir -p "$out/corpus"
# shellcheck disable=SC2086
"${CC:-clang}" -g -O1 -fsanitize=fuzzer,address,undefined -fno-sanitize-recover=all ${FUZZ_CFLAGS:-} \
  -I "$root/c_lib" "$root/test/fuzz/lexer_fuzz.c" "$root/c_lib/lexer.c" "$root/test/stock/stock_lexer.c" \
  -o "$out/lexer_fuzz"
exec "$out/lexer_fuzz" -dict="$root/test/fuzz/json.dict" "$out/corpus" "$@"
