A compiler for a kernel of Scheme.

Currently compiles a subset supporting

 - Basic arithmetic
 - Symbols
 - First class continuations
 - Tail calls
 - Closures
 - Mutation (which works with closures)

The runtime system currently is the simplest possible implementation.
In particular, the current implementation of GC is rather leaky now,
as it only collects a certain subset of garbage for simplicity.
This will improve over time.


## What's in c_of_scheme?
The full listing of primitive operations,

 - `+ - * /` with the obvious meanings
 - `display` which prints any scheme value
 - `cons car cdr`
 - `eq?` for equality

All programs must be a series of `define`'s, to execute actual values, use

     (define _
        (display (really-cool-function 0)))

The same as you would do in SML.

## How do I run c_of_scheme?

To use `c_of_scheme`, simply run `make build` which will stick the rts in `~/.c_of_scheme` and give you
an excutable, `c_of_scheme` to run on a scheme file producing `a.out`.

## Can I Contribute?
Absolutely! `c_of_scheme` is so small to make it a nice platform for hacking on a simple compiler.

If you're interested in contributing anything, please email me at danny DOT gratzer AT gmail.com.
Some things that could be fun to work on include

 - A GC
 - Any optimizations
 - Adding some macros to make the generated C more legible
 - A better UI for the command line
 - Flags to print out the intermediate closure converted and CPSed code
 - A pretty printer for intermediate languages
 - A macro system

or anything else that interests you!

## How is `c_of_scheme` organized?

The compiler is under `src/`, the runtime system is under `c-bits/` and some
examples of compilable Scheme programs are under `test/`.
