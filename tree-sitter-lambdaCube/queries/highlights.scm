; highlights.scm

(comment_line) @comment.line
(comment_block) @comment.block
(sym) @variable
(nat) @constant.numeric.integer
(level_t) @type.builtin
(level_add) @operator
(universe) @type.builtin
(universe (levels) @punctuation)
(named) @keyword.storage.type
(named name: (sym) @variable)
(bracket) @punctuation.bracket
(lambda) @keyword.function
(lambda name: (sym) @variable.parameter)
(lambda erasure: (erased) @operator)
