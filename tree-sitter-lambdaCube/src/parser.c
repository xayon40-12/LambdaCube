#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 31
#define LARGE_STATE_COUNT 5
#define SYMBOL_COUNT 27
#define ALIAS_COUNT 0
#define TOKEN_COUNT 13
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
  anon_sym_COMMA = 7,
  anon_sym_PLUS = 8,
  anon_sym_POUNDU = 9,
  anon_sym_AT = 10,
  anon_sym_EQ = 11,
  anon_sym_SEMI = 12,
  sym_expr = 13,
  sym__comment = 14,
  sym_comment_block = 15,
  sym__end_comment = 16,
  sym__levels = 17,
  sym__level = 18,
  sym_level_add = 19,
  sym_universe = 20,
  sym_named = 21,
  sym__base_expr = 22,
  sym__pre_expr = 23,
  sym__post_expr = 24,
  sym__expr = 25,
  aux_sym__levels_repeat1 = 26,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_comment_line] = "comment_line",
  [anon_sym_LBRACE_DASH] = "{- ",
  [anon_sym_DASH_RBRACE] = " -}",
  [aux_sym__end_comment_token1] = "_end_comment_token1",
  [sym_sym] = "sym",
  [sym_nat] = "nat",
  [anon_sym_COMMA] = ",",
  [anon_sym_PLUS] = "+",
  [anon_sym_POUNDU] = "#U",
  [anon_sym_AT] = "@",
  [anon_sym_EQ] = " = ",
  [anon_sym_SEMI] = ";",
  [sym_expr] = "expr",
  [sym__comment] = "_comment",
  [sym_comment_block] = "comment_block",
  [sym__end_comment] = "_end_comment",
  [sym__levels] = "_levels",
  [sym__level] = "_level",
  [sym_level_add] = "level_add",
  [sym_universe] = "universe",
  [sym_named] = "named",
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
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_POUNDU] = anon_sym_POUNDU,
  [anon_sym_AT] = anon_sym_AT,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [sym_expr] = sym_expr,
  [sym__comment] = sym__comment,
  [sym_comment_block] = sym_comment_block,
  [sym__end_comment] = sym__end_comment,
  [sym__levels] = sym__levels,
  [sym__level] = sym__level,
  [sym_level_add] = sym_level_add,
  [sym_universe] = sym_universe,
  [sym_named] = sym_named,
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
  [8] = 7,
  [9] = 9,
  [10] = 9,
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
  [24] = 6,
  [25] = 5,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
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
        '+', 24,
        ',', 23,
        '-', 7,
        ';', 28,
        '@', 26,
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
        '+', 24,
        ',', 23,
        '-', 7,
        ';', 28,
        '@', 26,
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
        '+', 24,
        ',', 23,
        '-', 8,
        ';', 28,
        '=', 3,
        '@', 26,
        '{', 9,
      );
      if (('\t' <= lookahead && lookahead <= '\r')) SKIP(1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(22);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(21);
      END_STATE();
    case 3:
      if (lookahead == ' ') ADVANCE(27);
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
      if (lookahead == 'U') ADVANCE(25);
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
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(anon_sym_POUNDU);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(anon_sym_AT);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(anon_sym_SEMI);
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
  [7] = {.lex_state = 6},
  [8] = {.lex_state = 6},
  [9] = {.lex_state = 6},
  [10] = {.lex_state = 6},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 6},
  [25] = {.lex_state = 6},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_comment_line] = ACTIONS(1),
    [anon_sym_LBRACE_DASH] = ACTIONS(1),
    [anon_sym_DASH_RBRACE] = ACTIONS(1),
    [sym_sym] = ACTIONS(1),
    [sym_nat] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_POUNDU] = ACTIONS(1),
    [anon_sym_AT] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
  },
  [1] = {
    [sym_expr] = STATE(29),
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(16),
    [sym_named] = STATE(16),
    [sym__base_expr] = STATE(16),
    [sym__pre_expr] = STATE(16),
    [sym__post_expr] = STATE(16),
    [sym__expr] = STATE(16),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(7),
    [sym_nat] = ACTIONS(7),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
  },
  [2] = {
    [sym_expr] = STATE(23),
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(16),
    [sym_named] = STATE(16),
    [sym__base_expr] = STATE(16),
    [sym__pre_expr] = STATE(16),
    [sym__post_expr] = STATE(16),
    [sym__expr] = STATE(16),
    [ts_builtin_sym_end] = ACTIONS(13),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(7),
    [sym_nat] = ACTIONS(7),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_SEMI] = ACTIONS(13),
  },
  [3] = {
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(11),
    [sym_named] = STATE(11),
    [sym__base_expr] = STATE(11),
    [sym__pre_expr] = STATE(11),
    [sym__post_expr] = STATE(11),
    [sym__expr] = STATE(11),
    [ts_builtin_sym_end] = ACTIONS(15),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(17),
    [sym_nat] = ACTIONS(17),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
    [anon_sym_SEMI] = ACTIONS(15),
  },
  [4] = {
    [sym_expr] = STATE(28),
    [sym__comment] = STATE(3),
    [sym_comment_block] = STATE(3),
    [sym_universe] = STATE(16),
    [sym_named] = STATE(16),
    [sym__base_expr] = STATE(16),
    [sym__pre_expr] = STATE(16),
    [sym__post_expr] = STATE(16),
    [sym__expr] = STATE(16),
    [sym_comment_line] = ACTIONS(3),
    [anon_sym_LBRACE_DASH] = ACTIONS(5),
    [sym_sym] = ACTIONS(7),
    [sym_nat] = ACTIONS(7),
    [anon_sym_POUNDU] = ACTIONS(9),
    [anon_sym_AT] = ACTIONS(11),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 1,
    ACTIONS(19), 8,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      sym_sym,
      sym_nat,
      anon_sym_POUNDU,
      anon_sym_AT,
      anon_sym_SEMI,
  [11] = 1,
    ACTIONS(21), 8,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      sym_sym,
      sym_nat,
      anon_sym_POUNDU,
      anon_sym_AT,
      anon_sym_SEMI,
  [22] = 5,
    ACTIONS(25), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(27), 1,
      anon_sym_DASH_RBRACE,
    STATE(6), 1,
      sym__end_comment,
    ACTIONS(23), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(10), 2,
      sym__comment,
      sym_comment_block,
  [40] = 5,
    ACTIONS(25), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(31), 1,
      anon_sym_DASH_RBRACE,
    STATE(24), 1,
      sym__end_comment,
    ACTIONS(29), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(9), 2,
      sym__comment,
      sym_comment_block,
  [58] = 5,
    ACTIONS(25), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(33), 1,
      anon_sym_DASH_RBRACE,
    STATE(25), 1,
      sym__end_comment,
    ACTIONS(29), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(9), 2,
      sym__comment,
      sym_comment_block,
  [76] = 5,
    ACTIONS(25), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(35), 1,
      anon_sym_DASH_RBRACE,
    STATE(5), 1,
      sym__end_comment,
    ACTIONS(23), 2,
      sym_comment_line,
      aux_sym__end_comment_token1,
    STATE(10), 2,
      sym__comment,
      sym_comment_block,
  [94] = 2,
    STATE(21), 2,
      sym__comment,
      sym_comment_block,
    ACTIONS(37), 4,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
  [105] = 2,
    ACTIONS(41), 1,
      anon_sym_PLUS,
    ACTIONS(39), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [116] = 3,
    ACTIONS(45), 1,
      anon_sym_COMMA,
    STATE(14), 1,
      aux_sym__levels_repeat1,
    ACTIONS(43), 4,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
  [129] = 3,
    ACTIONS(45), 1,
      anon_sym_COMMA,
    STATE(15), 1,
      aux_sym__levels_repeat1,
    ACTIONS(47), 4,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
  [142] = 3,
    ACTIONS(51), 1,
      anon_sym_COMMA,
    STATE(15), 1,
      aux_sym__levels_repeat1,
    ACTIONS(49), 4,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
  [155] = 4,
    ACTIONS(5), 1,
      anon_sym_LBRACE_DASH,
    ACTIONS(56), 1,
      sym_comment_line,
    ACTIONS(54), 2,
      ts_builtin_sym_end,
      anon_sym_SEMI,
    STATE(21), 2,
      sym__comment,
      sym_comment_block,
  [170] = 4,
    ACTIONS(58), 1,
      sym_sym,
    ACTIONS(60), 1,
      sym_nat,
    STATE(20), 1,
      sym__levels,
    STATE(13), 2,
      sym__level,
      sym_level_add,
  [184] = 1,
    ACTIONS(62), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [192] = 1,
    ACTIONS(49), 5,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_COMMA,
      anon_sym_SEMI,
  [200] = 1,
    ACTIONS(64), 4,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
  [207] = 1,
    ACTIONS(66), 4,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
  [214] = 3,
    ACTIONS(58), 1,
      sym_sym,
    ACTIONS(68), 1,
      sym_nat,
    STATE(19), 2,
      sym__level,
      sym_level_add,
  [225] = 1,
    ACTIONS(70), 4,
      ts_builtin_sym_end,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_SEMI,
  [232] = 1,
    ACTIONS(72), 4,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_DASH_RBRACE,
      aux_sym__end_comment_token1,
  [239] = 1,
    ACTIONS(74), 4,
      sym_comment_line,
      anon_sym_LBRACE_DASH,
      anon_sym_DASH_RBRACE,
      aux_sym__end_comment_token1,
  [246] = 1,
    ACTIONS(76), 1,
      anon_sym_EQ,
  [250] = 1,
    ACTIONS(78), 1,
      sym_nat,
  [254] = 1,
    ACTIONS(80), 1,
      anon_sym_SEMI,
  [258] = 1,
    ACTIONS(82), 1,
      ts_builtin_sym_end,
  [262] = 1,
    ACTIONS(84), 1,
      sym_sym,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(5)] = 0,
  [SMALL_STATE(6)] = 11,
  [SMALL_STATE(7)] = 22,
  [SMALL_STATE(8)] = 40,
  [SMALL_STATE(9)] = 58,
  [SMALL_STATE(10)] = 76,
  [SMALL_STATE(11)] = 94,
  [SMALL_STATE(12)] = 105,
  [SMALL_STATE(13)] = 116,
  [SMALL_STATE(14)] = 129,
  [SMALL_STATE(15)] = 142,
  [SMALL_STATE(16)] = 155,
  [SMALL_STATE(17)] = 170,
  [SMALL_STATE(18)] = 184,
  [SMALL_STATE(19)] = 192,
  [SMALL_STATE(20)] = 200,
  [SMALL_STATE(21)] = 207,
  [SMALL_STATE(22)] = 214,
  [SMALL_STATE(23)] = 225,
  [SMALL_STATE(24)] = 232,
  [SMALL_STATE(25)] = 239,
  [SMALL_STATE(26)] = 246,
  [SMALL_STATE(27)] = 250,
  [SMALL_STATE(28)] = 254,
  [SMALL_STATE(29)] = 258,
  [SMALL_STATE(30)] = 262,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_named, 5, 0, 1),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expr, 1, 0, 0),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__end_comment, 2, 0, 0),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comment_block, 2, 0, 0),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [27] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [29] = {.entry = {.count = 1, .reusable = false}}, SHIFT(9),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(5),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__pre_expr, 2, 0, 0),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__level, 1, 0, 0),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__levels, 1, 0, 0),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__levels, 2, 0, 0),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__levels_repeat1, 2, 0, 0),
  [51] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__levels_repeat1, 2, 0, 0), SHIFT_REPEAT(22),
  [54] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expr, 1, 0, 0),
  [56] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [58] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [60] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_level_add, 3, 0, 0),
  [64] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_universe, 2, 0, 0),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__post_expr, 2, 0, 0),
  [68] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [70] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_named, 6, 0, 2),
  [72] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comment_block, 2, 0, 0),
  [74] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__end_comment, 2, 0, 0),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [82] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
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
