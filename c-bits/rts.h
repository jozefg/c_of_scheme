#ifndef RTS
#define RTS
#include "types.h"
/* The interface to the RTS system of c_of_scheme.
   All operations except test may exit the program
   with a type error or similar.
*/
/* User primitives, these are wrapped by implicit
   scheme top level declaratios in Scheme.

   All of these should allocate memory and will eventually
   require GC-ing
*/
scm_t mkInt(int i);
scm_t mkSym(char* s);
scm_t mkClos(int i, ...);
scm_t mkLam (scm_t, lam_t l);

scm_t scm_eq(scm_t, scm_t);
int scm_eq_raw(scm_t, scm_t);
scm_t display(scm_t);
void scm_apply(int i, scm_t f, ...) __attribute__((noreturn));

scm_t scm_plus(scm_t, scm_t);
scm_t scm_sub(scm_t, scm_t);
scm_t scm_mult(scm_t, scm_t);
scm_t scm_div(scm_t, scm_t);

scm_t scm_cons(scm_t, scm_t);
scm_t scm_car(scm_t);
scm_t scm_cdr(scm_t);


scm_t scm_stop();
void scm_halt(scm_t);


/* Closure related RTS stuff

   Closures should also register themselves with the GC
   to be collected when no longer useful. We can't lexical scope
   them because callCC is mean.
*/

scm_t scm_select_clos(int, scm_t);
scm_t scm_write_clos(scm_t, scm_t, scm_t);
scm_t scm_top_clos;
#endif
