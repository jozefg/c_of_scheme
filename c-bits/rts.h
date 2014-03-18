// #ifdef SCHEME_2_C_RTS
// #define SCHEME_2_C_RTS

/* The interface to the RTS system of scheme2c.
   All operations except test may exit the program
   with a type error or similar.
*/

typedef struct scheme_val* scm_t;
typedef void (*lam_t)(scm_t*);
/* User primitives, these are wrapped by implicit
   scheme top level declaratios in Scheme.

   All of these should allocate memory and will eventually
   require GC-ing
*/

scm_t mkInt(int i);
scm_t mkSym(char* s);
scm_t mkClos(int i, ...);
scm_t mkLam (lam_t l);

scm_t scm_eq(scm_t, scm_t);
int scm_eq_raw(scm_t, scm_t);
scm_t display(scm_t);
scm_t scm_apply(int i, scm_t f, ...);

scm_t scm_plus(scm_t, scm_t);
scm_t scm_sub(scm_t, scm_t);
scm_t scm_mult(scm_t, scm_t);
scm_t scm_div(scm_t, scm_t);

scm_t scm_cons(scm_t, scm_t);
scm_t scm_car(scm_t);
scm_t scm_cdr(scm_t);

void scm_halt(scm_t);

/* Closure related RTS stuff

   Closures should also register themselves with the GC
   to be collected when no longer useful. We can't lexical scope
   them because callCC is mean.
*/

scm_t scm_select_clos(scm_t, scm_t);
scm_t scm_write_clos(scm_t, scm_t, scm_t);
scm_t scm_top_clos;

// #endif
