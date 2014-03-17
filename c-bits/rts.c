#include "rts.h"
#include "stdio.h"
#include "stdlib.h"

struct scheme_val;
typedef struct clos {
  struct scheme_val* closed;
  int length;
  struct clos *parent_clos;
} clos_t;
  
typedef struct {
  struct scheme_val *head;
  struct scheme_val *tail;
} cons_t;

struct scheme_val {
  int state;
  union {
    int     scm_int;
    char*   scm_sym;
    cons_t* scm_cons;
    clos_t* scm_clos;
  } val;
};

scm_t scm_malloc(){
  return malloc(sizeof(struct scheme_val));
  /* Pretend there's a cool GC here */
}

scm_t mkInt(int i){
  scm_t scm_i = scm_malloc();
  struct scheme_val s = {.state = 0, {.scm_int = i}};
  *scm_i = s;
  return scm_i;
}
scm_t mkSym(char *c){
  scm_t scm_s = scm_malloc();
  struct scheme_val s = {.state = 1, {.scm_sym = c}};
  *scm_s = s;
  return scm_s;
}
