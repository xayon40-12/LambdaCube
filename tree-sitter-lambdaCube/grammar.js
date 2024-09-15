/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'lambdaCube',

  rules: {
    expr: $ => $._expr,

    _comment: $ => choice($.comment_line, $.comment_block),
    comment_line: $ => /-- [^\n]*/,
    comment_block: $ => seq('{- ', $._end_comment),
    _end_comment: $ => choice(
      ' -}',
      seq($._comment, $._end_comment),
      seq(/\s|\S/, $._end_comment)
    ),

    sym: $ => /[a-zA-Z]\w*/,
    nat: $ => /\d+/,
    level_t: $ => '#L',
    _levels: $ => seq($._level, repeat(seq(',', $._level))),
    _level: $ => choice($.sym, $.level_add, $.nat),
    level_add: $ => seq($.sym, '+', $.nat),
    universe: $ => seq('#U', $._levels),
    named: $ => prec.right(1, seq(
      '@',
      field('name', $.sym),
      ' = ',
      field('body', $.expr),
      ';',
      field('rest', optional($.expr)))),
    bracket: $ => seq('[', $._expr, ']'),
    
    _base_expr: $ => choice($.named, $.sym, $.nat, $.universe, $.level_t),
    _pre_expr: $ => prec.right(3, seq($._comment, $._expr)),
    _post_expr: $ => prec.left(2, seq($._expr, $._comment)),
    _expr: $ => choice($.bracket, $._comment, $._base_expr, $._pre_expr, $._post_expr),
  }
});
