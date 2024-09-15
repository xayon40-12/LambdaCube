; locals.scm

(sym) @local.reference
(named) @local.scope
(named
  name: (sym) @local.definition
  body: (_) @local.scope
  rest: (_) @local.scope)
(lambda) @local.scope
(lambda 
  name: (sym) @local.definition
  type: (_) @local.scope
  body: (_) @local.scope)
