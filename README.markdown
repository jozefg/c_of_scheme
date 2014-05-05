A compiler for a kernel of Scheme.

Currently compiles a subset supporting

 - Basic arithmetic
 - Symbols
 - First class continuations
 - Tail calls
 - Closures
 - Mutation (which works with closures)

The runtime system currently is the simplest possible implementation.
This will support garbage collection Real Soon Now.

The full listing of primitive operations,

 - `+ - * /` with the obvious meanings
 - `display` which prints any scheme value
 - `cons car cdr`
 - `eq?` for equality

All programs must be a series of `define`'s, to execute actual values, use

     (define _
        (display (really-cool-function 0)))

To use scheme2c, simply run `make build` which will stick the rts in `~/.scheme2c` and give you
an excutable, `scheme2c` to run on a scheme file producing `a.out`.
