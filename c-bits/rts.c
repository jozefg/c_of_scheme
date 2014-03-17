#include "rts.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

struct scheme_val;
typedef struct clos {
  struct scheme_val** closed;
  int length;
} clos_t;
  
typedef struct {
  struct scheme_val *head;
  struct scheme_val *tail;
} cons_t;

struct scheme_val {
  int state;
  union {
    int    scm_int;
    char*  scm_sym;
    cons_t scm_cons;
    clos_t scm_clos;
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

scm_t mkCons(scm_t h, scm_t t){
  scm_t scm_s = scm_malloc();
  struct scheme_val s = {.state = 2, {.scm_cons = {.head = h, .tail = t}}};
  *scm_s = s;
  return scm_s;
}

scm_t mkClos(int i, ...){
  va_list ap;
  int x;
  scm_t result = scm_malloc();
  scm_t *closed = malloc(sizeof(scm_t) * i);
  struct scheme_val s = {.state = 3, {.scm_clos = {.closed = NULL, .length = i}}};

  va_start(ap, i);
  for(x = 0; x < i; ++x){
    closed[x] = va_arg(ap, scm_t);
  }
  va_end(ap);
  s.val.scm_clos.closed = closed;
  *result = s;
  return result;
}

int scm_eq_raw(scm_t l, scm_t r){
  if(l->state != r->state)
    return 0;
  switch(l->state){
  case 0: return l->val.scm_int == r->val.scm_int;
  case 1: return strcmp(l->val.scm_sym, r->val.scm_sym);
  case 2: case 3: return l == r;
  }
  return 0;
}
scm_t scm_eq(scm_t l, scm_t r){
  return mkInt(scm_eq_raw(l, r));
}
