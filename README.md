# Dependent lambda encoding

This is my attempt to do a dependently type programming language. It is inspired by:  
- [Homotopy Type Theory](https://homotopytypetheory.org/book/)
- [Simpler, Easier!](https://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html?m=1)
- [From realizability to induction via dependent intersection](https://www.sciencedirect.com/science/article/pii/S0168007218300277?ref=pdf_download&fr=RR-2&rr=8a9706eecc573c95)

**Table of contents:**
- [Grammar](#Grammar)
- [Erasure](#erasure)
  - [Erasure mark on lambda](#Erasure-mark-on-lambda)
- [Cumulative Universes](#Cumulative-Universes)
  - [Level](Level)
- [Dependent intersection](#Dependent-intersection)
- [Beta equivalence](#Beta-equivalence)
- [Examples](#Examples)
  - [Todo](#Todo)
    - [Nat](#Nat)

## Grammar

In the definition of the grammar for this language, a symbol denoted by `S` corresponds to any non empty sequence of characters which does not include any whitespaces and which does not already appear as literal in the later grammar.
A context is a list of symbols.
A symbol `S` that appears in a context named `l` is denoted by `elem(S,l)`.
A symbol matching any symbol in the context `l` is denoted by `any(l)`.
An expression with an attached context `l` is denoted by `expr[l]`.
An expression with an available symbol `S` in addition to a context `l` is denoted by `expr[l,S]`.
A valid expression with context `l` is any sequence of character matching the following 8 lines, where any letter or symbol which was not introduced betneen backticks in the previous centenses has to be matched literally:
```
@S = expr[l]; expr[l,S]
#L
#U 'levels'
(S: expr[l]) -> expr[l,S]
expr[l] :> expr[l]
expr[l] expr[l]
any(l)
S: expr[l] /\ expr[l,S]
expr[l].1
expr[l].2
[expr[l]]
Beta expr[l] expr[l]
'expr[l]
```
The `;` can be replaced by a newline followed by at least one space.
Each line in order correspond to:
- declaration of a named expression that can be used in later expressions
- type of universe levels
- a universe whose rank correspond to the provided 'levels' which should be a comma separated list of level (see [Level](#Level)) which represent the maximum of each provided levels.
- decleration of a dependent lambda (symbol: type) -> expr
- typing of the right expression with the first one as its type
- application, left associative
- a symbol available in the context l
- a dependent intersection
- term of the dependent intersection converted to the type of the left part
- term `t` of the dependent intersection converted to the type of the right part (where the symbol `S` is replaced by `t.1`)
- parens to force a particular association order
- type conversion of a value between two beta-equivalent types
- an erased expression (can be used for the type in a lambda)

## Erasure
The erasure correpsond to the computation part of an expression. For instance, the identity is the function that returns its input unchanged. However, in the formalism proporsed here, additional components are needed to preperly type the identity function:
```
(i: '#L) -> (T: '#U i) -> (t: T) -> T :> t
```
a level `i` and a generic type `T` are needed to type the identity function. They are therefor marked as erased with `'`. Erasing this expression will remove all symbols marked as erased and will strip all the remaning symbols of their types. After erasure, the above identity function becomes:
```
t -> t
```
which is the identity function in untyped lambda calculus.

### Erasure mark on lambda

When a lambda (a term or a type) is marked to be erased, the mark is propagated inside the lambda to the type of the symbol and to the expression. In `(P: '(t: T) -> #U i) -> (t: T) -> '#U i :> P 't` we see that to provide `t` to `P` we most mark it a erased so that it would have the erased type `'T` due to the propagation of the erased mark in `P` from `'(t: T) -> #U i` to `(t: 'T) -> '#U i`.

## Cumulative Universes
A universe is a type of type indexed by a level. For a level `l`, the corresponding universe is denoted by `U l`.
A valid level can be: a literal positive integer, a symbol of the special level type `L`, the sum of a level with a literal positive integer, the max of two levels.

Cumulative universes are such that for any level `l1` strictly smaller than `l2` we have `U l1` of type `U l2`. A non-cumulative universe system would only allow that `U l1` is of type `U l2` when `l2 = l1 + 1`.

```
(l1: #L) -> (l2: #L) -> (A: #U l1) -> (B: #U l2) -> U l1,l2
```
where `l1,l2` correspond to the maximum of `l1` and `l2`

### Level

A valid level can be a symbol of type `#L`, a literal natural number, the sum of a level and a literal natural number, or the maximum of two levels.

## Dependent intersection
For a value `a` of type `A` and a value `b` of type `B a`, the pair `a ^ b` is of type `a: A /\ B a` if the erasure of `a` and `b` are equal.
A value `x` of type `a: A /\ B a` can be changed to be of type `A` with the notation `x.1` and it can be changed to be of type `B x.1` with notation `x.2`. In both cases, the value can be returned to the original intersection type by appending a `.0`. Thus `x.1.0` and `x.2.0` are both of type `a: A /\ B a`.

## Beta equivalence
Two values (term or type) are beta-equivalent if their erasure is the same. for instance:
```
[c2nat [CSucc n]].1 => [CSucc n] Nat Succ Zero => [P -> s -> z -> s [n P s z]] Nat Succ Zero => Succ [n Nat Succ Zero] => P -> s -> z -> s [[n Nat Succ Zero] P s z]
CSucc [c2nat n].1 => CSucc [n Nat Succ Zero] => P -> s -> z -> s [[n Nat Succ Zero] P s z]
```
where the symbol `=>` here separates the transformation steps.

If two types `A` and `B` are beta-equivalent `A =_beta B`, then a value `a` of type `A` can be converted to type `B` with `Beta a`.

## Examples

See files with `.lam3` extension in the [math](math/) folder.

### Todo

The symbol '???' is used when a proof is not finished to be written. It is not valid in the language but just used here for unfinished work.
#### Nat
```
@Nat = (n: CNat /\ INat n);
@Zero = Nat :> CZero ^ IZero;
@Succ = (n: Nat) -> Nat :> [CSucc n.1] ^ [ISucc n.1 n.2];

@c2nat = (i: '#L) -> (n: CNat i) -> Nat :> n Nat Succ Zero;
@c2nat_reflection = (n: Nat) -> (i: #L) -> Equal i Nat [c2nat n.1] n :> ???;
@Ind = (i: '#L) -> (P: '(n: Nat) -> #U i) -> (s: (n: 'Nat) -> (p: P n) -> P [Succ n]) -> (z: P Zero) -> (n: Nat) -> P n :> ???;
```
