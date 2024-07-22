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
Pair = (A: *) -> (B: A->*) 
        -> (P: (A: *) -> (a: A) -> (B: A->*) -> *) -> (p: (a: A) -> (b: B a) -> P A a B)
pair = (A: *) -> (B: A->*) -> (a: A) -> (b: B a) ->
        -> (P: (A: *) -> (a: A) -> (B: A->*) -> *) -> (p: (a: A) -> (b: B a) -> P A a B)
        -> p a b
first = (A: *) -> (B: A->*) -> (p: Pair A B) 
        -> p ((A: *) -> (a: A) -> (B: A->*) -> A) ((a: A) -> (b: B a) -> a)
second = (A: *) -> (B: A->*) -> (p: Pair A B) -> p ((A: *) -> (a: A)
         -> (B: A->*) -> B a) ((a: A) -> (b: B a) -> b)
```

## Nat

```
CNat = (P: *) -> (s: P -> P) -> (z: P) -> P
CZero = (P: *) -> (s: P -> P) -> (z: P) -> z
CSucc = (n: CNat) -> (P: *) -> (s: P -> P) -> (z: P) -> s (n P s z)

INat = (n: CNat) -> (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> P n
IZero = (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> z
ISucc = (n: CNat) -> (in: INat n) -> (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> s n (in n P s z)

Nat = Pair CNat (n: CNat -> INat n) 
Zero = pair CNat (n: CNat -> INat n) CZero IZero
Zucc = (n: Nat) -> pair CNat (n: CNat -> INat n) (CZucc (first CNat (n: CNat -> INat n) n)) (ISucc (first CNat (n: CNat -> INat n) n) (second CNat (n: CNat -> INat n)))
```
