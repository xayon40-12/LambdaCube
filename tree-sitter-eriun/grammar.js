/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "eriun",

  rules: {
    expr: $ => $._expr,

    _comment: $ => choice($.commentLine, $.commentBlock),
    commentLine: $ => /-- [^\n]*/,
    commentBlock: $ => seq("{- ", $._endComment),
    _endComment: $ => choice(
      " -}",
      seq($._comment, $._endComment),
      seq(/\s|\S/, $._endComment)
    ),

    sym: $ => /[a-zA-Z]\w*/,
    nat: $ => /\d+/,
    levelT: $ => "#L",
    levels: $ => seq($._level, repeat(seq(",", $._level))),
    _level: $ => choice($.sym, $.levelAdd, $.nat),
    levelAdd: $ => seq($.sym, "+", $.nat),
    universe: $ => seq("#U", $.levels),
    named: $ => prec.right(9, seq(
      "@",
      field("name", $.sym),
      ": ",
      field("type", $._expr),
      " = ",
      field("body", $._expr),
      ";",
      field("rest", $._expr))),
    bracket: $ => seq("[", $._expr, "]"),
    erased: $ => "'",
    lambda: $ => seq(
      "(", 
      optional(seq(
        field("name", $.sym),
        ":"
      )),
      field("type", $._expr),
      ")",
      field("body", $._expr)
    ),
    erasedLambda: $ => seq(
      "<", 
      optional(seq(
        field("name", $.sym),
        ":"
      )),
      field("type", $._expr),
      ">",
      field("body", $._expr)
    ),
    intersectionT: $ => seq(
      "(",
      field("name", $.sym),
      ":",
      field("type1", $._expr),
      "/\\",
      field("type2", $._expr),
      ")"
      ),
    opIntersection: $ => prec.right(5, seq($._expr, "^", $._expr)),
    opApp: $ => prec.left(6, seq(
      field("fun", $._expr),
      field("erasure", optional($.erased)),
      field("value", $._expr)
    )),
    
    _baseExpr: $ => choice($.named, $.sym, $.nat, $.universe, $.levelT),
    _preComments: $ => prec.right(9, seq($._comment, $._expr)),
    _postComments: $ => prec.left(8, seq($._expr, $._comment)),
    postIntersection: $ => prec.left(7, seq($._expr, /\.[12]/)),
    _expr: $ => choice(
      $.bracket,
      $._comment,
      $._baseExpr,
      $._preComments,
      $._postComments,
      $.postIntersection,
      $.lambda,
      $.erasedLambda,
      $.intersectionT,
      $.opIntersection,
      $.opApp,
    ),
  }
});
