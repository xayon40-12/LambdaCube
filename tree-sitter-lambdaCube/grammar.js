/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'lambdaCube',

  rules: {
    source_file: $ => $._expr,
    _expr: $ => repeat1($._base_expr),
    _base_expr: $ => choice($._comment, $.sym, $.nat),
    _comment: $ => choice($.comment_line, $.comment_block),
    comment_line: $ => /-- [^\n]*/,
    comment_block: $ => seq('{- ', $._end_comment),
    _end_comment: $ => choice(
      ' -}',
      seq($._comment, $._end_comment),
      seq(/\s|\S/, $._end_comment)
    ),
    sym: $ => /[a-zA-Z]\w+/,
    nat: $ => /\d+/
  }
});
