# Hasche
An interpreter for a subset of Scheme written in Haskell

## Features
- REPL Session
- `call/cc`
- Legacy macros

## Usage
### Build
```sh
$ cabal build
```

### Run
```sh
$ cabal exec hasche -- exec programs/hello.scm
# or
$ cabal exec hasche -- repl
```

## List of available syntax, macros & functions
### Special forms
```
define, define-macro, set!, lambda, if, quote, quasiquote, unquote, unquote-splicing
```
### Basic macros
```
begin, when, unless, and, or, cond, let, let*, letrec, do
```
### Basic functions
```
eval, apply
```
```
null?, pair?, boolean?, number?, string?, symbol?, procedure?
```
```
not
```
```
+, -, *, /, =, <, <=, >, >=
```
```
list?, car, cdr, cons, list, length, memq, last, append, set-car!, set-cdr!, caar, cadr, cdar, cddr, map, for-each
```
```
string-append, symbol->string, string->symbol, string->number, number->string
```
```
eq?, neq?, equal?
```
```
open-input-file, open-output-file, close-input-port, close-output-port, read, display, write, newline, load
```
```
call/cc, call-with-current-continuation
```
```
gensym
```
### Lazy evaluation
```
delay, force, delay-force, promise-done?
```
