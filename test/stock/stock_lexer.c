/*
 * The lexer as of 537a43a7, before the performance changes, under renamed symbols.
 * The differential tests and the fuzz harness compare it with c_lib/lexer.c.
 */
#define lex_json      stock_lex_json
#define handle_number stock_handle_number
#define handle_string stock_handle_string
#include "lexer_537a43a7.c"
