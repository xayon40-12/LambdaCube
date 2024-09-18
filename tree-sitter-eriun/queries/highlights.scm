; highlights.scm

(commentLine) @comment.line
(commentBlock) @comment.block
(sym) @variable
(nat) @constant.numeric.integer
(levelT) @type.builtin
(levelAdd) @operator
(universe) @type.builtin
(universe (levels) @punctuation)
(named) @keyword.storage.type
(named name: (sym) @variable)
(bracket) @punctuation.bracket
(lambda) @keyword.function
(lambda name: (sym) @variable.parameter)
(lambda erasure: (erased) @operator)
(opTyped) @operator
(opIntersection) @operator
(intersectionT) @keyword.function
(intersectionT name: (sym) @variable.parameter)
