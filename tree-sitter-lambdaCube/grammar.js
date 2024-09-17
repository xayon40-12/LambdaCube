/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "lambdaCube",

  rules: {
    expr: $ => $._expr,

    _comment: $ => choice($.comment_line, $.comment_block),
    comment_line: $ => /-- [^\n]*/,
    comment_block: $ => seq("{- ", $._end_comment),
    _end_comment: $ => choice(
      " -}",
      seq($._comment, $._end_comment),
      seq(/\s|\S/, $._end_comment)
    ),

    sym: $ => /[a-zA-Z]\w*/,
    nat: $ => /\d+/,
    level_t: $ => "#L",
    levels: $ => seq($._level, repeat(seq(",", $._level))),
    _level: $ => choice($.sym, $.level_add, $.nat),
    level_add: $ => seq($.sym, "+", $.nat),
    universe: $ => seq("#U", $.levels),
    named: $ => prec.right(9, seq(
      "@",
      field("name", $.sym),
      " = ",
      field("body", $._expr),
      ";",
      field("rest", optional($._expr)))),
    bracket: $ => seq("[", $._expr, "]"),
    erased: $ => "'",
    lambda: $ => seq(
      "(", 
      field("name", $.sym),
      ":",
      field("type", $._expr),
      ")",
      field("erasure", optional($.erased)),
      "->",
      field("body", $._expr)
    ),
    opTyped: $ => prec.left(4, seq($._expr, ":>", $._expr)),
    opApp: $ => prec.left(5, seq($._expr, optional($.erased), $._expr)),
    
    _base_expr: $ => choice($.named, $.sym, $.nat, $.universe, $.level_t),
    _per_comments: $ => prec.right(8, seq($._comment, $._expr)),
    _post_comments: $ => prec.left(7, seq($._expr, $._comment)),
    _expr: $ => choice(
      $.bracket,
      $._comment,
      $._base_expr,
      $._per_comments,
      $._post_comments,
      $.lambda,
      $.opTyped,
      $.opApp,
    ),
  }
});
