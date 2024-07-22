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
Pair: *
  = (A: *) -> (B: A->*) -> 
    (P: (A: *) -> (a: A) -> (B: A->*) -> *) -> (p: (a: A) -> (b: B a) -> P A a B)
pair:
  (A: *) -> (B: A->*) -> (a: A) -> (b: B a) ->
  (P: (A: *) -> (a: A) -> (B: A->*) -> *) -> (p: (a: A) -> (b: B a) -> P A a B) -> P A a B
  = p a b
first:
  (A: *) -> (B: A->*) -> (p: Pair A B) -> A
  = p ((A: *) -> (a: A) -> (B: A->*) -> A) ((a: A) -> (b: B a) -> a)
second:
  (A: *) -> (B: A->*) -> (p: Pair A B) -> B (first A B p)
  = p ((A: *) -> (a: A) -> (B: A->*) -> B a) ((a: A) -> (b: B a) -> b)
```

## Nat

```
CNat: * = (P: *) -> (s: P -> P) -> (z: P) -> P
CZero: (P: *) -> (s: P -> P) -> (z: P) -> P = z
CSucc: (n: CNat) -> (P: *) -> (s: P -> P) -> (z: P) -> P = s (n P s z)

INat: * = (n: CNat) -> (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> P n
IZero: (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> P CZero = z
ISucc: (n: CNat) -> (in: INat n) -> (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> P (CSucc n) = s n (in n P s z)

Nat: * = Pair CNat (n: CNat -> INat n) 
nat: (c: CNat) -> (i: INat c) -> Nat = pair CNat (n: CNat -> INat n) c i
c: (n: Nat) -> CNat = (first CNat (n: CNat -> INat n) n)
i: (n: Nat) -> INat = (second CNat (n: CNat -> INat n) n)

Zero: Nat = pair CNat (n: CNat -> INat n) CZero IZero
Succ: (n: Nat) -> Nat = nat (CSucc (c n)) (ISucc (c n) (i n))
Ind: (P: Nat -> *) -> (s: (n: Nat) -> P n -> P (Succ n)) -> (z: P Zero) -> (n: Nat) -> P n
  = ???
```
