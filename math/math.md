# Dependent lambda encoding

```
expr[e] =>
    | lambda => (a: expr[e]) -> expr[e,a]
    | application => expr[e] expr[e]
    | universe => U<int>

expr1: U<i> => ((a: expr1) -> expr2): U<i+1>
* = U 1
```


## Dependent pair
```
TPair = (A: *) -> (B: A->*) 
        -> (P: (A: *) -> (a: A) -> (B: A->*) -> *) -> (p: (a: A) -> (b: B a) -> P A a B)
Pair = (A: *) -> (B: A->*) -> (a: A) -> (b: B a) ->
        -> (P: (A: *) -> (a: A) -> (B: A->*) -> *) -> (p: (a: A) -> (b: B a) -> P A a B)
        -> p a b
first = (A: *) -> (B: A->*) -> (p: TPair A B) 
        -> p ((A: *) -> (a: A) -> (B: A->*) -> A) ((a: A) -> (b: B a) -> a)
second = (A: *) -> (B: A->*) -> (p: TPair A B) -> p ((A: *) -> (a: A)
         -> (B: A->*) -> B a) ((a: A) -> (b: B a) -> b)
```
