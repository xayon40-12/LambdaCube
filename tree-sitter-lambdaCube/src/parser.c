#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 34
#define LARGE_STATE_COUNT 6
#define SYMBOL_COUNT 31
#define ALIAS_COUNT 0
#define TOKEN_COUNT 16
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 3
#define MAX_ALIAS_SEQUENCE_LENGTH 6
#define PRODUCTION_ID_COUNT 3

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
  sym_expr = 16,
  sym__comment = 17,
  sym_comment_block = 18,
  sym__end_comment = 19,
  sym__levels = 20,
  sym__level = 21,
  sym_level_add = 22,
  sym_universe = 23,
  sym_named = 24,
  sym_bracket = 25,
  sym__base_expr = 26,
  sym__pre_expr = 27,
  sym__post_expr = 28,
  sym__expr = 29,
  aux_sym__levels_repeat1 = 30,
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
  [sym_expr] = "expr",
  [sym__comment] = "_comment",
  [sym_comment_block] = "comment_block",
  [sym__end_comment] = "_end_comment",
  [sym__levels] = "_levels",
  [sym__level] = "_level",
  [sym_level_add] = "level_add",
  [sym_universe] = "universe",
  [sym_named] = "named",
  [sym_bracket] = "bracket",
  [sym__base_expr] = "_base_expr",
  [sym__pre_expr] = "_pre_expr",
  [sym__post_expr] = "_post_expr",
  [sym__expr] = "_expr",
  [aux_sym__levels_repeat1] = "_levels_repeat1",
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
  [sym_expr] = sym_expr,
  [sym__comment] = sym__comment,
  [sym_comment_block] = sym_comment_block,
  [sym__end_comment] = sym__end_comment,
  [sym__levels] = sym__levels,
  [sym__level] = sym__level,
  [sym_level_add] = sym_level_add,
  [sym_universe] = sym_universe,
  [sym_named] = sym_named,
  [sym_bracket] = sym_bracket,
  [sym__base_expr] = sym__base_expr,
  [sym__pre_expr] = sym__pre_expr,
  [sym__post_expr] = sym__post_expr,
  [sym__expr] = sym__expr,
  [aux_sym__levels_repeat1] = aux_sym__levels_repeat1,
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
  [sym__levels] = {
    .visible = false,
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
  [aux_sym__levels_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum ts_field_identifiers {
  field_body = 1,
  field_name = 2,
  field_rest = 3,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_body] = "body",
  [field_name] = "name",
  [field_rest] = "rest",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 2},
  [2] = {.index = 2, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_body, 3},
    {field_name, 1},
  [2] =
    {field_body, 3},
    {field_name, 1},
    {field_rest, 5},
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
  [14] = 10,
  [15] = 15,
  [16] = 12,
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
  [27] = 6,
  [28] = 7,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
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
        '+', 25,
        ',', 24,
        '-', 7,
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
        '+', 25,
        ',', 24,
        '-', 7,
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
        '+', 25,
        ',', 24,
        '-', 8,
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
      END_STATE();
    case 8:
      if (lookahead == '-') ADVANCE(4);
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
  [10] = {.lex_state = 6},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 6},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 6},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 6},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 6},
  [28] = {.lex_state = 6},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
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
  },
  [1] = {
    [sym_expr] = STATE(31),
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(15),
    [sym_named] = STATE(15),
    [sym_bracket] = STATE(15),
    [sym__base_expr] = STATE(15),
    [sym__pre_expr] = STATE(15),
    [sym__post_expr] = STATE(15),
    [sym__expr] = STATE(15),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(7),
    [sym_nat] = ACTIONS(7),
    [sym_level_t] = ACTIONS(7),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
  },
  [2] = {
    [sym_expr] = STATE(24),
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(15),
    [sym_named] = STATE(15),
    [sym_bracket] = STATE(15),
    [sym__base_expr] = STATE(15),
    [sym__pre_expr] = STATE(15),
    [sym__post_expr] = STATE(15),
    [sym__expr] = STATE(15),
    [ts_builtin_sym_end] = ACTIONS(15),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(7),
    [sym_nat] = ACTIONS(7),
    [sym_level_t] = ACTIONS(7),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_SEMI] = ACTIONS(15),
    [anon_sym_LBRACK] = ACTIONS(13),
    [anon_sym_RBRACK] = ACTIONS(15),
  },
  [3] = {
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(8),
    [sym_named] = STATE(8),
    [sym_bracket] = STATE(8),
    [sym__base_expr] = STATE(8),
    [sym__pre_expr] = STATE(8),
    [sym__post_expr] = STATE(8),
    [sym__expr] = STATE(8),
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
  },
  [4] = {
    [sym_expr] = STATE(32),
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(15),
    [sym_named] = STATE(15),
    [sym_bracket] = STATE(15),
    [sym__base_expr] = STATE(15),
    [sym__pre_expr] = STATE(15),
    [sym__post_expr] = STATE(15),
    [sym__expr] = STATE(15),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(7),
    [sym_nat] = ACTIONS(7),
    [sym_level_t] = ACTIONS(7),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
  },
  [5] = {
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(20),
    [sym_named] = STATE(20),
    [sym_bracket] = STATE(20),
    [sym__base_expr] = STATE(20),
    [sym__pre_expr] = STATE(20),
    [sym__post_expr] = STATE(20),
    [sym__expr] = STATE(20),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(21),
    [sym_nat] = ACTIONS(21),
    [sym_level_t] = ACTIONS(21),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_LBRACK] = ACTIONS(13),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 1,
    ACTIONS(23), 11,
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
  [14] = 1,
    ACTIONS(25), 11,
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
  [28] = 2,
    STATE(25), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(27), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [40] = 3,
    ACTIONS(31), 1,
      anon_sym_COMMA,
    STATE(17), 1,
      aux_sym__levels_repeat1,
    ACTIONS(29), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [54] = 5,
    ACTIONS(35), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(37), 1,
      anon_sym_DASH_RBRACE,
    STATE(7), 1,
      sym__end_comment,
    ACTIONS(33), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(10), 2,
      sym__comment,
      sym_comment_block,
  [72] = 2,
    ACTIONS(41), 1,
      anon_sym_PLUS,
    ACTIONS(39), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [84] = 5,
    ACTIONS(35), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(45), 1,
      anon_sym_DASH_RBRACE,
    STATE(27), 1,
      sym__end_comment,
    ACTIONS(43), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(14), 2,
      sym__comment,
      sym_comment_block,
  [102] = 3,
    ACTIONS(31), 1,
      anon_sym_COMMA,
    STATE(9), 1,
      aux_sym__levels_repeat1,
    ACTIONS(47), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [116] = 5,
    ACTIONS(35), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(49), 1,
      anon_sym_DASH_RBRACE,
    STATE(28), 1,
      sym__end_comment,
    ACTIONS(43), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(14), 2,
      sym__comment,
      sym_comment_block,
  [134] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(53), 1,
      sym_comment_line,
    STATE(25), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(51), 3,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [150] = 5,
    ACTIONS(35), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(55), 1,
      anon_sym_DASH_RBRACE,
    STATE(6), 1,
      sym__end_comment,
    ACTIONS(33), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(10), 2,
      sym__comment,
      sym_comment_block,
  [168] = 3,
    ACTIONS(59), 1,
      anon_sym_COMMA,
    STATE(17), 1,
      aux_sym__levels_repeat1,
    ACTIONS(57), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [182] = 1,
    ACTIONS(57), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [191] = 1,
    ACTIONS(62), 6,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [200] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(53), 1,
      sym_comment_line,
    ACTIONS(64), 1,
      anon_sym_RBRACK,
    STATE(25), 2,
      sym__comment,
      sym_comment_block,
  [214] = 1,
    ACTIONS(66), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [222] = 1,
    ACTIONS(68), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [230] = 4,
    ACTIONS(70), 1,
      sym_sym,
    ACTIONS(72), 1,
      sym_nat,
    STATE(22), 1,
      sym__levels,
    STATE(13), 2,
      sym__level,
      sym_level_add,
  [244] = 1,
    ACTIONS(74), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [252] = 1,
    ACTIONS(76), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
      anon_sym_RBRACK,
  [260] = 3,
    ACTIONS(70), 1,
      sym_sym,
    ACTIONS(78), 1,
      sym_nat,
    STATE(18), 2,
      sym__level,
      sym_level_add,
  [271] = 1,
    ACTIONS(80), 4,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_DASH_RBRACE,
      aux_sym__end_comment_token1,
  [278] = 1,
    ACTIONS(82), 4,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_DASH_RBRACE,
      aux_sym__end_comment_token1,
  [285] = 1,
    ACTIONS(84), 1,
      sym_sym,
  [289] = 1,
    ACTIONS(86), 1,
      sym_nat,
  [293] = 1,
    ACTIONS(88), 1,
      ts_builtin_sym_end,
  [297] = 1,
    ACTIONS(90), 1,
      anon_sym_SEMI,
  [301] = 1,
    ACTIONS(92), 1,
      anon_sym_EQ,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(6)] = 0,
  [SMALL_STATE(7)] = 14,
  [SMALL_STATE(8)] = 28,
  [SMALL_STATE(9)] = 40,
  [SMALL_STATE(10)] = 54,
  [SMALL_STATE(11)] = 72,
  [SMALL_STATE(12)] = 84,
  [SMALL_STATE(13)] = 102,
  [SMALL_STATE(14)] = 116,
  [SMALL_STATE(15)] = 134,
  [SMALL_STATE(16)] = 150,
  [SMALL_STATE(17)] = 168,
  [SMALL_STATE(18)] = 182,
  [SMALL_STATE(19)] = 191,
  [SMALL_STATE(20)] = 200,
  [SMALL_STATE(21)] = 214,
  [SMALL_STATE(22)] = 222,
  [SMALL_STATE(23)] = 230,
  [SMALL_STATE(24)] = 244,
  [SMALL_STATE(25)] = 252,
  [SMALL_STATE(26)] = 260,
  [SMALL_STATE(27)] = 271,
  [SMALL_STATE(28)] = 278,
  [SMALL_STATE(29)] = 285,
  [SMALL_STATE(30)] = 289,
  [SMALL_STATE(31)] = 293,
  [SMALL_STATE(32)] = 297,
  [SMALL_STATE(33)] = 301,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_named, 5, 0, 1),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expr, 1, 0, 0),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comment_block, 2, 0, 0),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__end_comment, 2, 0, 0),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__pre_expr, 2, 0, 0),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__levels, 2, 0, 0),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [37] = {.entry = {.count = 1, .reusable = false}}, SHIFT(7),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__level, 1, 0, 0),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [43] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [45] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__levels, 1, 0, 0),
  [49] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1, 0, 0),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [55] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__levels_repeat1, 2, 0, 0),
  [59] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__levels_repeat1, 2, 0, 0), SHIFT_REPEAT(26),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_level_add, 3, 0, 0),
  [64] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bracket, 3, 0, 0),
  [68] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_universe, 2, 0, 0),
  [70] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [72] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [74] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_named, 6, 0, 2),
  [76] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__post_expr, 2, 0, 0),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [80] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comment_block, 2, 0, 0),
  [82] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__end_comment, 2, 0, 0),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [88] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
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
