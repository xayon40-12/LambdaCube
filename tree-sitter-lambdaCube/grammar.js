/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'lambdaCube',

  rules: {
    source_file: $ => choice($.sym, $.nat),
    sym: $ => /[a-zA-Z]\w+/,
    nat: $ => /\d+/
  }
});
