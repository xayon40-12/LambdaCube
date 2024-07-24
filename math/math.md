# Dependent lambda encoding

## Grammar
```
expr[e] =>
    | sym => <name_literal n> where n in e
    | top_level => <name_literal n> = expr[e] <\n on ';'> expr[e,n]
    | lambda => (name_literal a: expr[e]) -> expr[e,a]
    | left_associative_application => expr[e] expr[e]
    | typed_expr_with_type_in_left_position => expr[e] :> expr[e]
    | universe => U int
    | generic_erased_universe_int => <int> expr[e,i]
```

## Rules
```
expr1: U<i>, expr2: U<j> => ((a: expr1) -> expr2): U<i>   or is it U<max(i,j)> ?
```
## Examples

The symbol '*' stands for 'U i' such that 'i' was guessed.

TODO: replace all the '*' by the correct Universe.

### Dependent pair
```
Pair = (A: *) -> (B: A -> *) -> * :>
    (P: (A: *) -> (a: A) -> (B: A -> *) -> *) -> (p: (a: A) -> (b: B a) -> P A a B)
pair = (A: *) -> (B: A -> *) -> (a: A) -> (b: B a) ->
  (P: (A: *) -> (a: A) -> (B: A -> *) -> *) -> (p: (a: A) -> (b: B a) -> P A a B) -> P A a B :>
  p a b
first = (A: *) -> (B: A -> *) -> (p: Pair A B) -> A :>
  p ((A: *) -> (a: A) -> (B: A -> *) -> A) ((a: A) -> (b: B a) -> a)
second (A: *) -> (B: A -> *) -> (p: Pair A B) -> B (first A B p) :>
  p ((A: *) -> (a: A) -> (B: A -> *) -> B a) ((a: A) -> (b: B a) -> b)
```

### Equal
```
Equal = <i> (A: U i) -> (a: A) -> (b: A) -> U (j+1) :>
  <j> (P: A -> U j) -> (p: P a) -> P b
Refl = <i> (A: U i) -> (a: A) -> Equal A a a :>
  <j> (P: A -> U j) -> (p: P a) -> p

symm = <i> (A: U i) -> (a: A) -> (b: A) -> (e: Equal A a b) -> Equal A b a :>
  <j> (P: A -> U j) -> e ((x: A) -> ((p: P x) -> U (j+1) :: P a) (Refl A a P)

```

### Nat

```
CNat = (P: *) -> (s: P -> P) -> (z: P) -> * :> P
CZero = (P: *) -> (s: P -> P) -> (z: P) -> CNat P s z :> z
CSucc = (n: CNat) -> (P: *) -> (s: P -> P) -> (z: P) -> CNat P s z :> s (n P s z)

INat = (n: CNat) -> (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> * :> P n
IZero = (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> INat CZero P s z :> z
ISucc = (n: CNat) -> (in: INat n) -> (P: CNat -> *) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> INat (CSucc n) P s z :> s n (in n P s z)

Nat = * :> Pair CNat (n: CNat -> INat n) 
nat = (c: CNat) -> (i: INat c) -> Nat :> pair CNat (n: CNat -> INat n) c i
c = (n: Nat) -> CNat :> (first CNat (n: CNat -> INat n) n)
i = (n: Nat) -> INat (c n) :> (second CNat (n: CNat -> INat n) n)

Zero = Nat :> pair CNat (n: CNat -> INat n) CZero IZero
Succ = (n: Nat) -> Nat :> nat (CSucc (c n)) (ISucc (c n) (i n))

c2nat = (n: CNat) -> Nat :> n Nat Succ Zero
c2nat_reflection = (n: Nat) -> Equal Nat (c2nat (c n)) n :> 
  i n ((n: CNat) -> Equal CNat (c (c2nat n)) n) ??? (Refl CNat CZero)

Ind = (P: Nat -> *) -> (s: (n: Nat) -> P n -> P (Succ n)) -> (z: P Zero) -> (n: Nat) -> P n :>
  ???
```
