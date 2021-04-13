# Mini-Scheme
A Mini-Scheme interpreter written in Haskell

## Mini-Scheme Specification
Mini-Scheme is a subset of the programming language Scheme

### Syntax
```
Toplevel ::= Exp
    | Define
    | (load String)

Define ::= (define Id Exp)
    | (define (Id Id* [. Id]) Body)

Exp ::= Const
    | Id
    | (lambda Arg Body)
    | (Exp Exp*)
    | (quote S-Exp)
    | (set! Id Exp)
    | (let [Id] Bindings Body)
    | (let* [Id] Bindings Body)
    | (letrec [Id] Bindings Body)
    | (if Exp Exp [Exp])
    | (cond (Exp Exp+)* [(else Exp+)])
    | (and Exp*)
    | (or Exp*)
    | (begin Exp*)
    | (do ((Id Exp Exp)*) (Exp Exp*) Body)

Body ::= Define* Exp+

Arg ::= Id
    | (Id* [Id . Id])

Bindings ::= ((Id Exp)*)

S-Exp ::= Const
    | Id
    | (S-Exp* [S-Exp . S-Exp])

Const ::= Num
    | Bool
    | String
    | ()

Num ::= number

Bool ::= '#t'
    | '#f'

String ::= string surrounded by "

Id ::= [0-9A-Za-z!$%&*+-./<=>?@^_]+
```

### Basic functions
```
number?, +, -, *, /, =, <, <=, >, >=
```
```
null?, pair?, list?, symbol?, car, cdr, cons, list, length, memq, last, append, set-car!, set-cdr!
```
```
boolean?, not
```
```
string?, string-append, symbol->string, string->symbol, string->number, number->string
```
```
procedure?
```
```
eq?, neq?, equal?
```
```
load
```

### Optional features
- [ ] Tail call optimization
- [ ] Macros
- [ ] call/cc
