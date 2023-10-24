# Scheme-ish

## Group Name

idk to be determined

- Ethan Zhang (ethanz4)
- Ian Chen (ianchen3)

## Project Overview

A simple, tree-walking interpreter for a subset of the [Scheme](https://en.wikipedia.org/wiki/Scheme_\(programming_language\)) programming language written in Rust.
For those unfamiliar with Scheme, it is a mostly-functional programming language in the Lisp family originally developed at MIT CSAIL, and
taught in the influential intro CS textbook (and MIT course of the same name) *Structure and Interpretation of Computer Programs (SICP)*.

We chose this project because Scheme is a very elegant language (that is very easy to parse) and is thus considered to be one of the
canonical languages for writing your first programming language interpreter.
Additionally, our high school taught out of the first half of SICP, so both of us already have some experience with Scheme.

## Technical Overview

There are three fundamental parts to every interpreter:
- the lexer, which transforms a sequence of characters (your code) into a sequence of tokens
- the parser, which takes a sequence of tokens and constructs an abstract syntax tree (AST)
- the evaluator, which walks the AST, evaluating the code step by step

As an additional component, we would like to include a repl (read-eval-print loop), as it is an essential part of the Scheme development workflow.

### Checkpoint 1

- Lexer (Converting S-expressions to tokens)
    - Tokens include things like parens, numbers, booleans, keywords, identifiers, etc.
- Parser (Create AST)
    - Create a CFG for the language
    - Write a simple parser for the CFG
- Evaluator:
    - Be able to evaluate simple scheme programs with the following features:
        - basic arithmetic, e.g.
          ```scheme
          (+ (/ (* 2 4) 2) (- 8 4))
          ```
        - variable definition, e.g.
          ```scheme
          (define x 5)
          (define y 10)
          (+ x y)
          ```
        - conditionals, e.g.
          ```scheme
          (define x 5)
          (define y 10)
          (if (< x y) x y)
          ```

### Checkpoint 2

- Evaluator:
    - lambdas, e.g.
      ```scheme
      ((lambda (x) (* x x)) 5)
      ```
    - other language features
- repl:
    - CLI

## Possible Challenges

- Creating the CFG for the language
- How do we represent a scope / current state / environment
- Interactivity + graceful error handling

## Potential References

- https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book.html
- https://www.scheme.com/tspl2d/grammar.html
- https://medium.com/@seanchen/building-a-scheme-to-webassembly-compiler-part-1-d423c2995386
- https://norvig.com/lispy.html
