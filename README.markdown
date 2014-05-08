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
this will improve over time.


## What's in scheme2c?
The full listing of primitive operations,

 - `+ - * /` with the obvious meanings
 - `display` which prints any scheme value
 - `cons car cdr`
 - `eq?` for equality

All programs must be a series of `define`'s, to execute actual values, use

     (define _
        (display (really-cool-function 0)))


## How do I run scheme2c?

To use scheme2c, simply run `make build` which will stick the rts in `~/.scheme2c` and give you
an excutable, `scheme2c` to run on a scheme file producing `a.out`.

## Can I Contribute?
Absolutely! `scheme2c` is so small to make it a nice platform for hacking on a simple compiler.

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
