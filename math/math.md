# Dependent lambda encoding

## Grammar

In the definition of the grammar for this language, a symbol denoted by `S` corresponds to any non empty sequence of characters which does not include any whitespaces and which does not already appear as literal in the later grammar.
A context is a list of symbols.
A symbol `S` that appears in a context named `l` is denoted by `elem(S,l)`.
A symbol matching any symbol in the context `l` is denoted by `any(l)`.
An expression with an attached context `l` is denoted by `expr[l]`.
An expression with an available symbol `S` in addition to a context `l` is denoted by `expr[l,S]`.
A valid expression with context `l` is any sequence of character matching the following 8 lines, where any letter or symbol which was not introduced betneen backticks in the previous centenses has to be matched literally:
```
S = expr[l]; expr[l,S]
<S> expr[l,N]
U S
U S+1
(S: expr[l]) -> expr[l,S]
expr[l] :> expr[l]
expr[l] expr[l]
any(l)
```
The `;` can be replaced by a newline followed by at least one space.
Each line in order correspond to:
- declaration of a named expression that can be used in later expressions
- declaration of a generic natural number literal used in universes
- a universe whose rank correspond to the symbol (this symbol must have been introduced by the declaration of a generic natural number)
- same as previous line but one rank higher
- decleration of a dependent lambda
- typing of the right expression with the first one as its type
- application
- a symbol available in the context l

## Dependent intersection
For a value `a` of type `A` and a value `b` of type `B a`, the pair `a ^ b` is of type `(a: A /\ B a)` if the erasure of `a` and `b` are equal.
A value `x` of type `(a: A /\ B a)` can be changed to be of type `A` with the notation `x.1` and it can be changed to be of type `B x.1` with notation `x.2`. In both cases, the value can be returned to the original intersection type by appending a `.0`. Thus `x.1.0` and `x.2.0` are both of type `(a: A /\ B a)`.

## Rules
Universes 'U i' are cumulative. So for any j > i, U i: U j.

```
expr1: U<i>, expr2: U<j> => ((a: expr1) -> expr2): U<i>   or is it U<max(i,j)> ?
```
## Examples

The symbol '???' is used when a proof is not finished to be written.

### Void
The empty type
```
Void = <i> (A: U i) -> A
```

### Union
```
Union = <i> (A: U i) -> (B: U i) -> (P: U i) -> (l: (a: A) -> P) -> (r: (b: B) -> P) -> P
Left = <i> (A: U i) -> (B: U i) -> (a: A) -> Union A B :> (P: U i) -> (l: (a: A) -> P) -> (r: (b: B) -> P) -> l a
Right = <i> (A: U i) -> (B: U i) -> (b: B) -> Union A B :> (P: U i) -> (l: (a: A) -> P) -> (r: (b: B) -> P) -> r b
```

### Dependent pair
```
Pair = <i> (A: U i) -> (B: A -> U i) -> U i+1 :>
  (P: U i) -> (p: (a: A) -> (b: B a) -> P) -> P
pair = <i> (A: U i) -> (B: A -> U i) -> (a: A) -> (b: B a) -> Pair A B :>
  (P: U i) -> (p: (a: A) -> (b: B a) -> P) -> p a b
first = <i> (A: U i) -> (B: A -> U i) -> (p: Pair A B) -> A :>
  p A ((a: A) -> (b: B a) -> a)
second = <i> (A: U i) -> (B: A -> U i) -> (p: Pair A B) -> B (first A B p) :>
  p (B a) ((a: A) -> (b: B a) -> b)
```

### Equal
```
Equal = <i> (A: U i) -> (a: A) -> (b: A) -> U i+1 :>
  (P: (a: A) -> U i) -> (p: P a) -> P b
Refl = <i> (A: U i) -> (a: A) -> Equal A a a :>
  (P: (a: A) -> U i) -> (p: P a) -> p

rho = <i> (T: U i) -> (t1: T) -> (t2: T) -> (e: Equal T t1 t2) -> (Tt: (t: T) -> U i) -> (t: Tt t1) -> Tt t2 :>
  e (P: (t: T) -> Tt t) t

symm = <i> (A: U i) -> (a: A) -> (b: A) -> (e: Equal A a b) -> Equal A b a :>
  (P: (a: A) -> U i) -> e ((x: A) -> ((p: P x) -> P a) (Refl A a P)


uniq = <i> (A: U i) -> (a: A) -> E = Equal A a a; (e: E) -> Equal E e (Refl A a) :>
  (P: (e: E) -> U i) -> (p: P e) -> ???

uniq_refl = <i> (A: U i) -> (a: A) -> (b: A) -> E = Equal A a b; (e: E) -> Equal E e (Refl A a) :>
  e (P': (c: A) -> (e: Equal A a c ) -> (P: E -> U i) -> (p: P e) -> P (Refl A a)) ((e: Equal A a a) -> (P: E -> U i) -> (p: P e) -> ???) e
```

### Nat
```
CNat = <i> (P: U i) -> (s: P -> P) -> (z: P) -> U i :> P
CZero = <i> (P: U i) -> (s: P -> P) -> (z: P) -> CNat P s z :> z
CSucc = (n: CNat) -> <i> (P: U i) -> (s: P -> P) -> (z: P) -> CNat P s z :> s (n P s z)

INat = (n: CNat) -> <i> (P: CNat -> U i) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> U i :> P n
IZero = <i> (P: CNat -> U i) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> INat CZero P s z :> z
ISucc = (n: CNat) -> (in: INat n) -> <i> (P: CNat -> U i) -> (s: (n: CNat) -> P n -> P (CSucc n)) -> (z: P CZero) -> INat (CSucc n) P s z :> s n (in n P s z)

Nat = (n: CNat /\ INat n)
Zero = Nat :> CZero ^ IZero
Succ = (n: Nat) -> Nat :> (CSucc n.1) ^ (ISucc n.1 n.2)

c2nat = (n: CNat) -> Nat :> n Nat Succ Zero
c2nat_reflection = (n: Nat) -> Equal Nat (c2nat n.1) n :> 
  n.2 ((n: CNat) -> Equal CNat (c2nat n).1 n) ((n: CNat) -> (p: Equal CNat (c2nat n).1 n) -> (P: (n: CNat) -> U i) -> p (n: CNat -> (p: P (c2nat (CSucc n)).1) -> P (CSucc n)) (c: )) (Refl CNat CZero)

Ind = <i> (P: Nat -> U i) -> (s: (n: Nat) -> P n -> P (Succ n)) -> (z: P Zero) -> (n: Nat) -> P n :>
  ???
```
