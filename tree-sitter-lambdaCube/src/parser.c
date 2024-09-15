#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 44
#define LARGE_STATE_COUNT 9
#define SYMBOL_COUNT 37
#define ALIAS_COUNT 0
#define TOKEN_COUNT 21
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 5
#define MAX_ALIAS_SEQUENCE_LENGTH 8
#define PRODUCTION_ID_COUNT 5

enum ts_symbol_identifiers {
  sym_comment_line = 1,
  anon_sym_LBRACE_DASH = 2,
  anon_sym_DASH_RBRACE = 3,
  aux_sym__end_comment_token1 = 4,
  sym_sym = 5,
  sym_nat = 6,
  sym_level_t = 7,
  anon_sym_COMMA = 8,
  anon_sym_PLUS = 9,
  anon_sym_POUNDU = 10,
  anon_sym_AT = 11,
  anon_sym_EQ = 12,
  anon_sym_SEMI = 13,
  anon_sym_LBRACK = 14,
  anon_sym_RBRACK = 15,
  sym_erased = 16,
  anon_sym_LPAREN = 17,
  anon_sym_COLON = 18,
  anon_sym_RPAREN = 19,
  anon_sym_DASH_GT = 20,
  sym_expr = 21,
  sym__comment = 22,
  sym_comment_block = 23,
  sym__end_comment = 24,
  sym_levels = 25,
  sym__level = 26,
  sym_level_add = 27,
  sym_universe = 28,
  sym_named = 29,
  sym_bracket = 30,
  sym_lambda = 31,
  sym__base_expr = 32,
  sym__pre_expr = 33,
  sym__post_expr = 34,
  sym__expr = 35,
  aux_sym_levels_repeat1 = 36,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_comment_line] = "comment_line",
  [anon_sym_LBRACE_DASH] = "{- ",
  [anon_sym_DASH_RBRACE] = " -}",
  [aux_sym__end_comment_token1] = "_end_comment_token1",
  [sym_sym] = "sym",
  [sym_nat] = "nat",
  [sym_level_t] = "level_t",
  [anon_sym_COMMA] = ",",
  [anon_sym_PLUS] = "+",
  [anon_sym_POUNDU] = "#U",
  [anon_sym_AT] = "@",
  [anon_sym_EQ] = " = ",
  [anon_sym_SEMI] = ";",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [sym_erased] = "erased",
  [anon_sym_LPAREN] = "(",
  [anon_sym_COLON] = ":",
  [anon_sym_RPAREN] = ")",
  [anon_sym_DASH_GT] = "->",
  [sym_expr] = "expr",
  [sym__comment] = "_comment",
  [sym_comment_block] = "comment_block",
  [sym__end_comment] = "_end_comment",
  [sym_levels] = "levels",
  [sym__level] = "_level",
  [sym_level_add] = "level_add",
  [sym_universe] = "universe",
  [sym_named] = "named",
  [sym_bracket] = "bracket",
  [sym_lambda] = "lambda",
  [sym__base_expr] = "_base_expr",
  [sym__pre_expr] = "_pre_expr",
  [sym__post_expr] = "_post_expr",
  [sym__expr] = "_expr",
  [aux_sym_levels_repeat1] = "levels_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_comment_line] = sym_comment_line,
  [anon_sym_LBRACE_DASH] = anon_sym_LBRACE_DASH,
  [anon_sym_DASH_RBRACE] = anon_sym_DASH_RBRACE,
  [aux_sym__end_comment_token1] = aux_sym__end_comment_token1,
  [sym_sym] = sym_sym,
  [sym_nat] = sym_nat,
  [sym_level_t] = sym_level_t,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_POUNDU] = anon_sym_POUNDU,
  [anon_sym_AT] = anon_sym_AT,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [sym_erased] = sym_erased,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [sym_expr] = sym_expr,
  [sym__comment] = sym__comment,
  [sym_comment_block] = sym_comment_block,
  [sym__end_comment] = sym__end_comment,
  [sym_levels] = sym_levels,
  [sym__level] = sym__level,
  [sym_level_add] = sym_level_add,
  [sym_universe] = sym_universe,
  [sym_named] = sym_named,
  [sym_bracket] = sym_bracket,
  [sym_lambda] = sym_lambda,
  [sym__base_expr] = sym__base_expr,
  [sym__pre_expr] = sym__pre_expr,
  [sym__post_expr] = sym__post_expr,
  [sym__expr] = sym__expr,
  [aux_sym_levels_repeat1] = aux_sym_levels_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_comment_line] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LBRACE_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym__end_comment_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_sym] = {
    .visible = true,
    .named = true,
  },
  [sym_nat] = {
    .visible = true,
    .named = true,
  },
  [sym_level_t] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_POUNDU] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [sym_erased] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [sym_expr] = {
    .visible = true,
    .named = true,
  },
  [sym__comment] = {
    .visible = false,
    .named = true,
  },
  [sym_comment_block] = {
    .visible = true,
    .named = true,
  },
  [sym__end_comment] = {
    .visible = false,
    .named = true,
  },
  [sym_levels] = {
    .visible = true,
    .named = true,
  },
  [sym__level] = {
    .visible = false,
    .named = true,
  },
  [sym_level_add] = {
    .visible = true,
    .named = true,
  },
  [sym_universe] = {
    .visible = true,
    .named = true,
  },
  [sym_named] = {
    .visible = true,
    .named = true,
  },
  [sym_bracket] = {
    .visible = true,
    .named = true,
  },
  [sym_lambda] = {
    .visible = true,
    .named = true,
  },
  [sym__base_expr] = {
    .visible = false,
    .named = true,
  },
  [sym__pre_expr] = {
    .visible = false,
    .named = true,
  },
  [sym__post_expr] = {
    .visible = false,
    .named = true,
  },
  [sym__expr] = {
    .visible = false,
    .named = true,
  },
  [aux_sym_levels_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum ts_field_identifiers {
  field_body = 1,
  field_erasure = 2,
  field_name = 3,
  field_rest = 4,
  field_type = 5,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_body] = "body",
  [field_erasure] = "erasure",
  [field_name] = "name",
  [field_rest] = "rest",
  [field_type] = "type",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 2},
  [2] = {.index = 2, .length = 3},
  [3] = {.index = 5, .length = 3},
  [4] = {.index = 8, .length = 4},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_body, 3},
    {field_name, 1},
  [2] =
    {field_body, 3},
    {field_name, 1},
    {field_rest, 5},
  [5] =
    {field_body, 6},
    {field_name, 1},
    {field_type, 3},
  [8] =
    {field_body, 7},
    {field_erasure, 5},
    {field_name, 1},
    {field_type, 3},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 19,
  [22] = 20,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 9,
  [35] = 10,
  [36] = 36,
  [37] = 37,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 43,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(11);
      ADVANCE_MAP(
        ' ', 2,
        '#', 10,
        '\'', 32,
        '(', 33,
        ')', 35,
        '+', 25,
        ',', 24,
        '-', 7,
        ':', 34,
        ';', 29,
        '@', 27,
        '[', 30,
        ']', 31,
        '{', 9,
      );
      if (('\t' <= lookahead && lookahead <= '\r')) SKIP(0);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(22);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(21);
      END_STATE();
    case 1:
      ADVANCE_MAP(
        ' ', 2,
        '#', 10,
        '\'', 32,
        '(', 33,
        ')', 35,
        '+', 25,
        ',', 24,
        '-', 7,
        ':', 34,
        ';', 29,
        '@', 27,
        '[', 30,
        ']', 31,
        '{', 9,
      );
      if (('\t' <= lookahead && lookahead <= '\r')) SKIP(1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(22);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(21);
      END_STATE();
    case 2:
      ADVANCE_MAP(
        ' ', 2,
        '#', 10,
        '\'', 32,
        '(', 33,
        ')', 35,
        '+', 25,
        ',', 24,
        '-', 8,
        ':', 34,
        ';', 29,
        '=', 3,
        '@', 27,
        '[', 30,
        ']', 31,
        '{', 9,
      );
      if (('\t' <= lookahead && lookahead <= '\r')) SKIP(1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(22);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(21);
      END_STATE();
    case 3:
      if (lookahead == ' ') ADVANCE(28);
      END_STATE();
    case 4:
      if (lookahead == ' ') ADVANCE(12);
      END_STATE();
    case 5:
      if (lookahead == ' ') ADVANCE(13);
      END_STATE();
    case 6:
      if (lookahead == ' ') ADVANCE(17);
      if (lookahead == '-') ADVANCE(18);
      if (lookahead == '{') ADVANCE(20);
      if (('\t' <= lookahead && lookahead <= '\r')) ADVANCE(16);
      if (lookahead != 0) ADVANCE(15);
      END_STATE();
    case 7:
      if (lookahead == '-') ADVANCE(4);
      if (lookahead == '>') ADVANCE(36);
      END_STATE();
    case 8:
      if (lookahead == '-') ADVANCE(4);
      if (lookahead == '>') ADVANCE(36);
      if (lookahead == '}') ADVANCE(14);
      END_STATE();
    case 9:
      if (lookahead == '-') ADVANCE(5);
      END_STATE();
    case 10:
      if (lookahead == 'L') ADVANCE(23);
      if (lookahead == 'U') ADVANCE(26);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(sym_comment_line);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(12);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_LBRACE_DASH);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_DASH_RBRACE);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == ' ') ADVANCE(17);
      if (lookahead == '-') ADVANCE(18);
      if (lookahead == '{') ADVANCE(20);
      if (('\t' <= lookahead && lookahead <= '\r')) ADVANCE(16);
      if (lookahead != 0) ADVANCE(15);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == ' ') ADVANCE(17);
      if (lookahead == '-') ADVANCE(19);
      if (lookahead == '{') ADVANCE(20);
      if (('\t' <= lookahead && lookahead <= '\r')) ADVANCE(16);
      if (lookahead != 0) ADVANCE(15);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == '-') ADVANCE(4);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == '-') ADVANCE(4);
      if (lookahead == '}') ADVANCE(14);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == '-') ADVANCE(5);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(sym_sym);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(21);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(sym_nat);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(22);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym_level_t);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(anon_sym_POUNDU);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_AT);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(sym_erased);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 6},
  [20] = {.lex_state = 6},
  [21] = {.lex_state = 6},
  [22] = {.lex_state = 6},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 6},
  [35] = {.lex_state = 6},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_comment_line] = ACTIONS(1),
    [anon_sym_LBRACE_DASH] = ACTIONS(1),
    [anon_sym_DASH_RBRACE] = ACTIONS(1),
    [sym_sym] = ACTIONS(1),
    [sym_nat] = ACTIONS(1),
    [sym_level_t] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_POUNDU] = ACTIONS(1),
    [anon_sym_AT] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [sym_erased] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
  },
  [1] = {
    [sym_expr] = STATE(42),
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(28),
    [sym_named] = STATE(28),
    [sym_bracket] = STATE(28),
    [sym_lambda] = STATE(28),
    [sym__base_expr] = STATE(28),
    [sym__pre_expr] = STATE(28),
    [sym__post_expr] = STATE(28),
    [sym__expr] = STATE(28),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(7),
    [sym_nat] = ACTIONS(7),
    [sym_level_t] = ACTIONS(7),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
  [2] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(18),
    [sym_named] = STATE(18),
    [sym_bracket] = STATE(18),
    [sym_lambda] = STATE(18),
    [sym__base_expr] = STATE(18),
    [sym__pre_expr] = STATE(18),
    [sym__post_expr] = STATE(18),
    [sym__expr] = STATE(18),
    [ts_builtin_sym_end] = ACTIONS(17),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(19),
    [sym_nat] = ACTIONS(19),
    [sym_level_t] = ACTIONS(19),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_SEMI] = ACTIONS(17),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_RBRACK] = ACTIONS(17),
    [anon_sym_LPAREN] = ACTIONS(15),
    [anon_sym_RPAREN] = ACTIONS(17),
  },
  [3] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(17),
    [sym_named] = STATE(17),
    [sym_bracket] = STATE(17),
    [sym_lambda] = STATE(17),
    [sym__base_expr] = STATE(17),
    [sym__pre_expr] = STATE(17),
    [sym__post_expr] = STATE(17),
    [sym__expr] = STATE(17),
    [ts_builtin_sym_end] = ACTIONS(21),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(23),
    [sym_nat] = ACTIONS(23),
    [sym_level_t] = ACTIONS(23),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_SEMI] = ACTIONS(21),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_RBRACK] = ACTIONS(21),
    [anon_sym_LPAREN] = ACTIONS(15),
    [anon_sym_RPAREN] = ACTIONS(21),
  },
  [4] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(30),
    [sym_named] = STATE(30),
    [sym_bracket] = STATE(30),
    [sym_lambda] = STATE(30),
    [sym__base_expr] = STATE(30),
    [sym__pre_expr] = STATE(30),
    [sym__post_expr] = STATE(30),
    [sym__expr] = STATE(30),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(25),
    [sym_nat] = ACTIONS(25),
    [sym_level_t] = ACTIONS(25),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
  [5] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(29),
    [sym_named] = STATE(29),
    [sym_bracket] = STATE(29),
    [sym_lambda] = STATE(29),
    [sym__base_expr] = STATE(29),
    [sym__pre_expr] = STATE(29),
    [sym__post_expr] = STATE(29),
    [sym__expr] = STATE(29),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(27),
    [sym_nat] = ACTIONS(27),
    [sym_level_t] = ACTIONS(27),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
  [6] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(31),
    [sym_named] = STATE(31),
    [sym_bracket] = STATE(31),
    [sym_lambda] = STATE(31),
    [sym__base_expr] = STATE(31),
    [sym__pre_expr] = STATE(31),
    [sym__post_expr] = STATE(31),
    [sym__expr] = STATE(31),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(29),
    [sym_nat] = ACTIONS(29),
    [sym_level_t] = ACTIONS(29),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
  [7] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(15),
    [sym_named] = STATE(15),
    [sym_bracket] = STATE(15),
    [sym_lambda] = STATE(15),
    [sym__base_expr] = STATE(15),
    [sym__pre_expr] = STATE(15),
    [sym__post_expr] = STATE(15),
    [sym__expr] = STATE(15),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(31),
    [sym_nat] = ACTIONS(31),
    [sym_level_t] = ACTIONS(31),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
  [8] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(16),
    [sym_named] = STATE(16),
    [sym_bracket] = STATE(16),
    [sym_lambda] = STATE(16),
    [sym__base_expr] = STATE(16),
    [sym__pre_expr] = STATE(16),
    [sym__post_expr] = STATE(16),
    [sym__expr] = STATE(16),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(33),
    [sym_nat] = ACTIONS(33),
    [sym_level_t] = ACTIONS(33),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 1,
    ACTIONS(35), 13,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      sym_sym,
      sym_nat,
      sym_level_t,
      anon_sym_POUNDU,
      anon_sym_AT,
      anon_sym_SEMI,
      anon_sym_LBRACK,
      anon_sym_RBRACK,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [16] = 1,
    ACTIONS(37), 13,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      sym_sym,
      sym_nat,
      sym_level_t,
      anon_sym_POUNDU,
      anon_sym_AT,
      anon_sym_SEMI,
      anon_sym_LBRACK,
      anon_sym_RBRACK,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [32] = 2,
    ACTIONS(41), 1,
      anon_sym_PLUS,
    ACTIONS(39), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [45] = 3,
    ACTIONS(45), 1,
      anon_sym_COMMA,
    STATE(13), 1,
      aux_sym_levels_repeat1,
    ACTIONS(43), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [60] = 3,
    ACTIONS(49), 1,
      anon_sym_COMMA,
    STATE(13), 1,
      aux_sym_levels_repeat1,
    ACTIONS(47), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [75] = 3,
    ACTIONS(45), 1,
      anon_sym_COMMA,
    STATE(12), 1,
      aux_sym_levels_repeat1,
    ACTIONS(52), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [90] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(56), 1,
      sym_comment_line,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(54), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [107] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(56), 1,
      sym_comment_line,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(58), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [124] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(56), 1,
      sym_comment_line,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(60), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [141] = 2,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(62), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [154] = 5,
    ACTIONS(66), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(68), 1,
      anon_sym_DASH_RBRACE,
    STATE(9), 1,
      sym__end_comment,
    ACTIONS(64), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(20), 2,
      sym__comment,
      sym_comment_block,
  [172] = 5,
    ACTIONS(66), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(70), 1,
      anon_sym_DASH_RBRACE,
    STATE(10), 1,
      sym__end_comment,
    ACTIONS(64), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(20), 2,
      sym__comment,
      sym_comment_block,
  [190] = 5,
    ACTIONS(66), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(74), 1,
      anon_sym_DASH_RBRACE,
    STATE(34), 1,
      sym__end_comment,
    ACTIONS(72), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(22), 2,
      sym__comment,
      sym_comment_block,
  [208] = 5,
    ACTIONS(66), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(76), 1,
      anon_sym_DASH_RBRACE,
    STATE(35), 1,
      sym__end_comment,
    ACTIONS(72), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(22), 2,
      sym__comment,
      sym_comment_block,
  [226] = 1,
    ACTIONS(78), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [236] = 1,
    ACTIONS(47), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [246] = 1,
    ACTIONS(80), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [255] = 1,
    ACTIONS(82), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [264] = 1,
    ACTIONS(84), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [273] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(56), 1,
      sym_comment_line,
    ACTIONS(86), 1,
      ts_builtin_sym_end,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
  [287] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(56), 1,
      sym_comment_line,
    ACTIONS(88), 1,
      anon_sym_RBRACK,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
  [301] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(56), 1,
      sym_comment_line,
    ACTIONS(90), 1,
      anon_sym_SEMI,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
  [315] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(56), 1,
      sym_comment_line,
    ACTIONS(92), 1,
      anon_sym_RPAREN,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
  [329] = 4,
    ACTIONS(94), 1,
      sym_sym,
    ACTIONS(96), 1,
      sym_nat,
    STATE(25), 1,
      sym_levels,
    STATE(14), 2,
      sym__level,
      sym_level_add,
  [343] = 3,
    ACTIONS(94), 1,
      sym_sym,
    ACTIONS(98), 1,
      sym_nat,
    STATE(24), 2,
      sym__level,
      sym_level_add,
  [354] = 1,
    ACTIONS(100), 4,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_DASH_RBRACE,
      aux_sym__end_comment_token1,
  [361] = 1,
    ACTIONS(102), 4,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_DASH_RBRACE,
      aux_sym__end_comment_token1,
  [368] = 2,
    ACTIONS(104), 1,
      sym_erased,
    ACTIONS(106), 1,
      anon_sym_DASH_GT,
  [375] = 1,
    ACTIONS(108), 1,
      anon_sym_DASH_GT,
  [379] = 1,
    ACTIONS(110), 1,
      sym_sym,
  [383] = 1,
    ACTIONS(112), 1,
      sym_nat,
  [387] = 1,
    ACTIONS(114), 1,
      anon_sym_COLON,
  [391] = 1,
    ACTIONS(116), 1,
      anon_sym_EQ,
  [395] = 1,
    ACTIONS(118), 1,
      ts_builtin_sym_end,
  [399] = 1,
    ACTIONS(120), 1,
      sym_sym,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(9)] = 0,
  [SMALL_STATE(10)] = 16,
  [SMALL_STATE(11)] = 32,
  [SMALL_STATE(12)] = 45,
  [SMALL_STATE(13)] = 60,
  [SMALL_STATE(14)] = 75,
  [SMALL_STATE(15)] = 90,
  [SMALL_STATE(16)] = 107,
  [SMALL_STATE(17)] = 124,
  [SMALL_STATE(18)] = 141,
  [SMALL_STATE(19)] = 154,
  [SMALL_STATE(20)] = 172,
  [SMALL_STATE(21)] = 190,
  [SMALL_STATE(22)] = 208,
  [SMALL_STATE(23)] = 226,
  [SMALL_STATE(24)] = 236,
  [SMALL_STATE(25)] = 246,
  [SMALL_STATE(26)] = 255,
  [SMALL_STATE(27)] = 264,
  [SMALL_STATE(28)] = 273,
  [SMALL_STATE(29)] = 287,
  [SMALL_STATE(30)] = 301,
  [SMALL_STATE(31)] = 315,
  [SMALL_STATE(32)] = 329,
  [SMALL_STATE(33)] = 343,
  [SMALL_STATE(34)] = 354,
  [SMALL_STATE(35)] = 361,
  [SMALL_STATE(36)] = 368,
  [SMALL_STATE(37)] = 375,
  [SMALL_STATE(38)] = 379,
  [SMALL_STATE(39)] = 383,
  [SMALL_STATE(40)] = 387,
  [SMALL_STATE(41)] = 391,
  [SMALL_STATE(42)] = 395,
  [SMALL_STATE(43)] = 399,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expr, 1, 0, 0),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_named, 5, 0, 1),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comment_block, 2, 0, 0),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__end_comment, 2, 0, 0),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__level, 1, 0, 0),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_levels, 2, 0, 0),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_levels_repeat1, 2, 0, 0),
  [49] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_levels_repeat1, 2, 0, 0), SHIFT_REPEAT(33),
  [52] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_levels, 1, 0, 0),
  [54] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_lambda, 7, 0, 3),
  [56] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [58] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_lambda, 8, 0, 4),
  [60] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_named, 6, 0, 2),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__pre_expr, 2, 0, 0),
  [64] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [66] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [68] = {.entry = {.count = 1, .reusable = false}}, SHIFT(9),
  [70] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [72] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [74] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [76] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_level_add, 3, 0, 0),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_universe, 2, 0, 0),
  [82] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__post_expr, 2, 0, 0),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bracket, 3, 0, 0),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1, 0, 0),
  [88] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [100] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comment_block, 2, 0, 0),
  [102] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__end_comment, 2, 0, 0),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [116] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [118] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_lambdaCube(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
