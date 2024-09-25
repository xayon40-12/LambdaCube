; highlights.scm

(bracket) @punctuation.bracket
(sym) @variable
(commentLine) @comment.line
(commentBlock) @comment.block
(nat) @constant.numeric.integer
(universe) @type.builtin
(universe (levels) @punctuation)
(levelT) @type.builtin
(levelAdd) @operator
(named) @keyword.storage.type
(named name: (sym) @variable)
(lambda) @keyword.function
(lambda name: (sym) @variable.parameter)
(erasedLambda) @keyword.function
(erasedLambda name: (sym) @variable.parameter)
(intersectionT) @keyword.function
(intersectionT name: (sym) @variable.parameter)
(opTyped) @operator
(opIntersection) @operator
(postIntersection) @operator

