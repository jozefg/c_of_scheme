#ifndef TYPES
#define TYPES

typedef struct scheme_val* scm_t;
typedef void (*lam_t)(scm_t*) __attribute__((noreturn));

struct scheme_val;
typedef struct clos {
  struct scheme_val** closed;
  int length;
} clos_t;

typedef struct {
  struct scheme_val *head;
  struct scheme_val *tail;
} cons_t;

typedef struct {
  struct scheme_val *clos;
  lam_t fun;
} closed_lam_t;

struct scheme_val {
  int live;
  int state; // Used by mark-and-sweep GC
  union {
    int    scm_int;
    char*  scm_sym;
    cons_t scm_cons;
    clos_t scm_clos;
    closed_lam_t scm_lam;
  } val;
};
#endif
