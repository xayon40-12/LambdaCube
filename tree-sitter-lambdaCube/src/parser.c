#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 46
#define LARGE_STATE_COUNT 10
#define SYMBOL_COUNT 39
#define ALIAS_COUNT 0
#define TOKEN_COUNT 22
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
  anon_sym_COLON_GT = 21,
  sym_expr = 22,
  sym__comment = 23,
  sym_comment_block = 24,
  sym__end_comment = 25,
  sym_levels = 26,
  sym__level = 27,
  sym_level_add = 28,
  sym_universe = 29,
  sym_named = 30,
  sym_bracket = 31,
  sym_lambda = 32,
  sym_opTyped = 33,
  sym__base_expr = 34,
  sym__per_comments = 35,
  sym__post_comments = 36,
  sym__expr = 37,
  aux_sym_levels_repeat1 = 38,
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
  [anon_sym_COLON_GT] = ":>",
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
  [sym_opTyped] = "opTyped",
  [sym__base_expr] = "_base_expr",
  [sym__per_comments] = "_per_comments",
  [sym__post_comments] = "_post_comments",
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
  [anon_sym_COLON_GT] = anon_sym_COLON_GT,
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
  [sym_opTyped] = sym_opTyped,
  [sym__base_expr] = sym__base_expr,
  [sym__per_comments] = sym__per_comments,
  [sym__post_comments] = sym__post_comments,
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
  [anon_sym_COLON_GT] = {
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
  [sym_opTyped] = {
    .visible = true,
    .named = true,
  },
  [sym__base_expr] = {
    .visible = false,
    .named = true,
  },
  [sym__per_comments] = {
    .visible = false,
    .named = true,
  },
  [sym__post_comments] = {
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
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 25,
  [28] = 28,
  [29] = 24,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 11,
  [37] = 10,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 43,
  [44] = 44,
  [45] = 45,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(15);
      ADVANCE_MAP(
        ' ', 2,
        '#', 13,
        '\'', 36,
        '(', 37,
        ')', 40,
        '+', 29,
        ',', 28,
        '-', 8,
        ':', 39,
        ';', 33,
        '@', 31,
        '[', 34,
        ']', 35,
        '{', 10,
      );
      if (('\t' <= lookahead && lookahead <= '\r')) SKIP(0);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 1:
      ADVANCE_MAP(
        ' ', 2,
        '#', 13,
        '\'', 36,
        '(', 37,
        ')', 40,
        '+', 29,
        ',', 28,
        '-', 8,
        ':', 39,
        ';', 33,
        '@', 31,
        '[', 34,
        ']', 35,
        '{', 10,
      );
      if (('\t' <= lookahead && lookahead <= '\r')) SKIP(1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 2:
      ADVANCE_MAP(
        ' ', 2,
        '#', 13,
        '\'', 36,
        '(', 37,
        ')', 40,
        '+', 29,
        ',', 28,
        '-', 9,
        ':', 39,
        ';', 33,
        '=', 3,
        '@', 31,
        '[', 34,
        ']', 35,
        '{', 10,
      );
      if (('\t' <= lookahead && lookahead <= '\r')) SKIP(1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 3:
      if (lookahead == ' ') ADVANCE(32);
      END_STATE();
    case 4:
      if (lookahead == ' ') ADVANCE(16);
      END_STATE();
    case 5:
      if (lookahead == ' ') ADVANCE(17);
      END_STATE();
    case 6:
      if (lookahead == ' ') ADVANCE(21);
      if (lookahead == '-') ADVANCE(22);
      if (lookahead == '{') ADVANCE(24);
      if (('\t' <= lookahead && lookahead <= '\r')) ADVANCE(20);
      if (lookahead != 0) ADVANCE(19);
      END_STATE();
    case 7:
      if (lookahead == '-') ADVANCE(4);
      END_STATE();
    case 8:
      if (lookahead == '-') ADVANCE(4);
      if (lookahead == '>') ADVANCE(41);
      END_STATE();
    case 9:
      if (lookahead == '-') ADVANCE(4);
      if (lookahead == '>') ADVANCE(41);
      if (lookahead == '}') ADVANCE(18);
      END_STATE();
    case 10:
      if (lookahead == '-') ADVANCE(5);
      END_STATE();
    case 11:
      if (lookahead == ':') ADVANCE(38);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(11);
      END_STATE();
    case 12:
      if (lookahead == '>') ADVANCE(42);
      END_STATE();
    case 13:
      if (lookahead == 'L') ADVANCE(27);
      if (lookahead == 'U') ADVANCE(30);
      END_STATE();
    case 14:
      if (eof) ADVANCE(15);
      ADVANCE_MAP(
        '#', 13,
        '(', 37,
        ')', 40,
        '+', 29,
        ',', 28,
        '-', 7,
        ':', 12,
        ';', 33,
        '@', 31,
        '[', 34,
        ']', 35,
        '{', 10,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(14);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(sym_comment_line);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(16);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_LBRACE_DASH);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_DASH_RBRACE);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == ' ') ADVANCE(21);
      if (lookahead == '-') ADVANCE(22);
      if (lookahead == '{') ADVANCE(24);
      if (('\t' <= lookahead && lookahead <= '\r')) ADVANCE(20);
      if (lookahead != 0) ADVANCE(19);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == ' ') ADVANCE(21);
      if (lookahead == '-') ADVANCE(23);
      if (lookahead == '{') ADVANCE(24);
      if (('\t' <= lookahead && lookahead <= '\r')) ADVANCE(20);
      if (lookahead != 0) ADVANCE(19);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == '-') ADVANCE(4);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == '-') ADVANCE(4);
      if (lookahead == '}') ADVANCE(18);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(aux_sym__end_comment_token1);
      if (lookahead == '-') ADVANCE(5);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_sym);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_nat);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(sym_level_t);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_POUNDU);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_AT);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(sym_erased);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_COLON);
      if (lookahead == '>') ADVANCE(42);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_COLON_GT);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 14},
  [3] = {.lex_state = 14},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 14},
  [11] = {.lex_state = 14},
  [12] = {.lex_state = 14},
  [13] = {.lex_state = 14},
  [14] = {.lex_state = 14},
  [15] = {.lex_state = 14},
  [16] = {.lex_state = 14},
  [17] = {.lex_state = 14},
  [18] = {.lex_state = 14},
  [19] = {.lex_state = 14},
  [20] = {.lex_state = 14},
  [21] = {.lex_state = 14},
  [22] = {.lex_state = 14},
  [23] = {.lex_state = 14},
  [24] = {.lex_state = 6},
  [25] = {.lex_state = 6},
  [26] = {.lex_state = 14},
  [27] = {.lex_state = 6},
  [28] = {.lex_state = 14},
  [29] = {.lex_state = 6},
  [30] = {.lex_state = 14},
  [31] = {.lex_state = 14},
  [32] = {.lex_state = 14},
  [33] = {.lex_state = 14},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 6},
  [37] = {.lex_state = 6},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 11},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 0},
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
    [anon_sym_COLON_GT] = ACTIONS(1),
  },
  [1] = {
    [sym_expr] = STATE(43),
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(33),
    [sym_named] = STATE(33),
    [sym_bracket] = STATE(33),
    [sym_lambda] = STATE(33),
    [sym_opTyped] = STATE(33),
    [sym__base_expr] = STATE(33),
    [sym__per_comments] = STATE(33),
    [sym__post_comments] = STATE(33),
    [sym__expr] = STATE(33),
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
    [sym_universe] = STATE(16),
    [sym_named] = STATE(16),
    [sym_bracket] = STATE(16),
    [sym_lambda] = STATE(16),
    [sym_opTyped] = STATE(16),
    [sym__base_expr] = STATE(16),
    [sym__per_comments] = STATE(16),
    [sym__post_comments] = STATE(16),
    [sym__expr] = STATE(16),
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
    [anon_sym_COLON_GT] = ACTIONS(17),
  },
  [3] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(14),
    [sym_named] = STATE(14),
    [sym_bracket] = STATE(14),
    [sym_lambda] = STATE(14),
    [sym_opTyped] = STATE(14),
    [sym__base_expr] = STATE(14),
    [sym__per_comments] = STATE(14),
    [sym__post_comments] = STATE(14),
    [sym__expr] = STATE(14),
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
    [anon_sym_COLON_GT] = ACTIONS(21),
  },
  [4] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(12),
    [sym_named] = STATE(12),
    [sym_bracket] = STATE(12),
    [sym_lambda] = STATE(12),
    [sym_opTyped] = STATE(12),
    [sym__base_expr] = STATE(12),
    [sym__per_comments] = STATE(12),
    [sym__post_comments] = STATE(12),
    [sym__expr] = STATE(12),
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
    [sym_universe] = STATE(30),
    [sym_named] = STATE(30),
    [sym_bracket] = STATE(30),
    [sym_lambda] = STATE(30),
    [sym_opTyped] = STATE(30),
    [sym__base_expr] = STATE(30),
    [sym__per_comments] = STATE(30),
    [sym__post_comments] = STATE(30),
    [sym__expr] = STATE(30),
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
    [sym_opTyped] = STATE(31),
    [sym__base_expr] = STATE(31),
    [sym__per_comments] = STATE(31),
    [sym__post_comments] = STATE(31),
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
    [sym_universe] = STATE(32),
    [sym_named] = STATE(32),
    [sym_bracket] = STATE(32),
    [sym_lambda] = STATE(32),
    [sym_opTyped] = STATE(32),
    [sym__base_expr] = STATE(32),
    [sym__per_comments] = STATE(32),
    [sym__post_comments] = STATE(32),
    [sym__expr] = STATE(32),
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
    [sym_universe] = STATE(20),
    [sym_named] = STATE(20),
    [sym_bracket] = STATE(20),
    [sym_lambda] = STATE(20),
    [sym_opTyped] = STATE(20),
    [sym__base_expr] = STATE(20),
    [sym__per_comments] = STATE(20),
    [sym__post_comments] = STATE(20),
    [sym__expr] = STATE(20),
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
  [9] = {
    [sym__comment] = STATE(2),
    [sym_comment_block] = STATE(2),
    [sym_universe] = STATE(17),
    [sym_named] = STATE(17),
    [sym_bracket] = STATE(17),
    [sym_lambda] = STATE(17),
    [sym_opTyped] = STATE(17),
    [sym__base_expr] = STATE(17),
    [sym__per_comments] = STATE(17),
    [sym__post_comments] = STATE(17),
    [sym__expr] = STATE(17),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(35),
    [sym_nat] = ACTIONS(35),
    [sym_level_t] = ACTIONS(35),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 1,
    ACTIONS(37), 14,
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
      anon_sym_COLON_GT,
  [17] = 1,
    ACTIONS(39), 14,
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
      anon_sym_COLON_GT,
  [34] = 5,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(43), 1,
      sym_comment_line,
    ACTIONS(45), 1,
      anon_sym_COLON_GT,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(41), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [54] = 3,
    ACTIONS(49), 1,
      anon_sym_COMMA,
    STATE(19), 1,
      aux_sym_levels_repeat1,
    ACTIONS(47), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [70] = 2,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(51), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [84] = 2,
    ACTIONS(55), 1,
      anon_sym_PLUS,
    ACTIONS(53), 8,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [98] = 2,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(57), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [112] = 5,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(43), 1,
      sym_comment_line,
    ACTIONS(45), 1,
      anon_sym_COLON_GT,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(59), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
  [132] = 3,
    ACTIONS(63), 1,
      anon_sym_COMMA,
    STATE(18), 1,
      aux_sym_levels_repeat1,
    ACTIONS(61), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [148] = 3,
    ACTIONS(49), 1,
      anon_sym_COMMA,
    STATE(18), 1,
      aux_sym_levels_repeat1,
    ACTIONS(66), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [164] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(43), 1,
      sym_comment_line,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(68), 5,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [182] = 1,
    ACTIONS(61), 8,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [193] = 1,
    ACTIONS(70), 8,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [204] = 1,
    ACTIONS(72), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [214] = 5,
    ACTIONS(76), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(78), 1,
      anon_sym_DASH_RBRACE,
    STATE(11), 1,
      sym__end_comment,
    ACTIONS(74), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(25), 2,
      sym__comment,
      sym_comment_block,
  [232] = 5,
    ACTIONS(76), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(80), 1,
      anon_sym_DASH_RBRACE,
    STATE(10), 1,
      sym__end_comment,
    ACTIONS(74), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(25), 2,
      sym__comment,
      sym_comment_block,
  [250] = 1,
    ACTIONS(82), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [260] = 5,
    ACTIONS(76), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(86), 1,
      anon_sym_DASH_RBRACE,
    STATE(37), 1,
      sym__end_comment,
    ACTIONS(84), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(27), 2,
      sym__comment,
      sym_comment_block,
  [278] = 1,
    ACTIONS(88), 7,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
      anon_sym_RPAREN,
      anon_sym_COLON_GT,
  [288] = 5,
    ACTIONS(76), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(90), 1,
      anon_sym_DASH_RBRACE,
    STATE(36), 1,
      sym__end_comment,
    ACTIONS(84), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(27), 2,
      sym__comment,
      sym_comment_block,
  [306] = 5,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(43), 1,
      sym_comment_line,
    ACTIONS(45), 1,
      anon_sym_COLON_GT,
    ACTIONS(92), 1,
      anon_sym_RPAREN,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
  [323] = 5,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(43), 1,
      sym_comment_line,
    ACTIONS(45), 1,
      anon_sym_COLON_GT,
    ACTIONS(94), 1,
      anon_sym_SEMI,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
  [340] = 5,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(43), 1,
      sym_comment_line,
    ACTIONS(45), 1,
      anon_sym_COLON_GT,
    ACTIONS(96), 1,
      anon_sym_RBRACK,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
  [357] = 5,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(43), 1,
      sym_comment_line,
    ACTIONS(45), 1,
      anon_sym_COLON_GT,
    ACTIONS(98), 1,
      ts_builtin_sym_end,
    STATE(26), 2,
      sym__comment,
      sym_comment_block,
  [374] = 4,
    ACTIONS(100), 1,
      sym_sym,
    ACTIONS(102), 1,
      sym_nat,
    STATE(23), 1,
      sym_levels,
    STATE(13), 2,
      sym__level,
      sym_level_add,
  [388] = 3,
    ACTIONS(100), 1,
      sym_sym,
    ACTIONS(104), 1,
      sym_nat,
    STATE(21), 2,
      sym__level,
      sym_level_add,
  [399] = 1,
    ACTIONS(106), 4,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_DASH_RBRACE,
      aux_sym__end_comment_token1,
  [406] = 1,
    ACTIONS(108), 4,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_DASH_RBRACE,
      aux_sym__end_comment_token1,
  [413] = 2,
    ACTIONS(110), 1,
      sym_erased,
    ACTIONS(112), 1,
      anon_sym_DASH_GT,
  [420] = 1,
    ACTIONS(114), 1,
      sym_nat,
  [424] = 1,
    ACTIONS(116), 1,
      anon_sym_COLON,
  [428] = 1,
    ACTIONS(118), 1,
      anon_sym_DASH_GT,
  [432] = 1,
    ACTIONS(120), 1,
      anon_sym_EQ,
  [436] = 1,
    ACTIONS(122), 1,
      ts_builtin_sym_end,
  [440] = 1,
    ACTIONS(124), 1,
      sym_sym,
  [444] = 1,
    ACTIONS(126), 1,
      sym_sym,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(10)] = 0,
  [SMALL_STATE(11)] = 17,
  [SMALL_STATE(12)] = 34,
  [SMALL_STATE(13)] = 54,
  [SMALL_STATE(14)] = 70,
  [SMALL_STATE(15)] = 84,
  [SMALL_STATE(16)] = 98,
  [SMALL_STATE(17)] = 112,
  [SMALL_STATE(18)] = 132,
  [SMALL_STATE(19)] = 148,
  [SMALL_STATE(20)] = 164,
  [SMALL_STATE(21)] = 182,
  [SMALL_STATE(22)] = 193,
  [SMALL_STATE(23)] = 204,
  [SMALL_STATE(24)] = 214,
  [SMALL_STATE(25)] = 232,
  [SMALL_STATE(26)] = 250,
  [SMALL_STATE(27)] = 260,
  [SMALL_STATE(28)] = 278,
  [SMALL_STATE(29)] = 288,
  [SMALL_STATE(30)] = 306,
  [SMALL_STATE(31)] = 323,
  [SMALL_STATE(32)] = 340,
  [SMALL_STATE(33)] = 357,
  [SMALL_STATE(34)] = 374,
  [SMALL_STATE(35)] = 388,
  [SMALL_STATE(36)] = 399,
  [SMALL_STATE(37)] = 406,
  [SMALL_STATE(38)] = 413,
  [SMALL_STATE(39)] = 420,
  [SMALL_STATE(40)] = 424,
  [SMALL_STATE(41)] = 428,
  [SMALL_STATE(42)] = 432,
  [SMALL_STATE(43)] = 436,
  [SMALL_STATE(44)] = 440,
  [SMALL_STATE(45)] = 444,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expr, 1, 0, 0),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_named, 5, 0, 1),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__end_comment, 2, 0, 0),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comment_block, 2, 0, 0),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_lambda, 7, 0, 3),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_levels, 1, 0, 0),
  [49] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_named, 6, 0, 2),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__level, 1, 0, 0),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__per_comments, 2, 0, 0),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_lambda, 8, 0, 4),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_levels_repeat1, 2, 0, 0),
  [63] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_levels_repeat1, 2, 0, 0), SHIFT_REPEAT(35),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_levels, 2, 0, 0),
  [68] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_opTyped, 3, 0, 0),
  [70] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_level_add, 3, 0, 0),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_universe, 2, 0, 0),
  [74] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [76] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [78] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [80] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [82] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__post_comments, 2, 0, 0),
  [84] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [86] = {.entry = {.count = 1, .reusable = false}}, SHIFT(37),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bracket, 3, 0, 0),
  [90] = {.entry = {.count = 1, .reusable = false}}, SHIFT(36),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [98] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1, 0, 0),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [106] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comment_block, 2, 0, 0),
  [108] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__end_comment, 2, 0, 0),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [116] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [122] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [124] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
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
