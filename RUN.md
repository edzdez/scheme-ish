# Running Instructions

## About

We provide a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) functionality.
There is no need for newlines between code blocks.
Simply, enter the code that you want to interpret, or load any modules with the 
```scheme
(load "filename")
```
command.

## Start 

```shell
$ git clone https://github.com/edzdez/scheme-ish
$ cargo run --release
```

## Example Usage

```scheme
Welcome to scheme-ish!
Type :h for help
Don't bother trying to use arrow keys---MIT Scheme doesn't let you use them in the repl either
> (load "samples/mergesort.scm")
Unit
> (mergesort (list 3 1 4 1 5))
( 1 1 3 4 5 )
> (exit)
Goodbye!
```


