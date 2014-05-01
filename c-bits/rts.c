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

typedef struct {
  struct scheme_val *clos;
  lam_t fun;
} closed_lam_t;

struct scheme_val {
  int state;
  union {
    int    scm_int;
    char*  scm_sym;
    cons_t scm_cons;
    clos_t scm_clos;
    closed_lam_t scm_lam;
  } val;
};

scm_t scm_malloc(){
  return malloc(sizeof(struct scheme_val));
  /* Pretend there's a cool GC here */
}

scm_t mkInt(int i){
  scm_t scm_i = scm_malloc();
  struct scheme_val s = {.state = 0, {.scm_int = i}};
  memcpy(scm_i, &s, sizeof *scm_i);
  return scm_i;
}

scm_t mkSym(char *c){
  scm_t scm_s = scm_malloc();
  struct scheme_val s = {.state = 1, {.scm_sym = c}};
  memcpy(scm_s, &s, sizeof *scm_s);
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
  memcpy(result, &s, sizeof *result);
  return result;
}

scm_t mkLam(scm_t c, lam_t l){
  scm_t scm_s = scm_malloc();
  struct scheme_val s = {.state = 4, {.scm_lam = {c, l}}};
  memcpy(scm_s, &s, sizeof *scm_s);
  return scm_s;
}

int scm_eq_raw(scm_t l, scm_t r){
  if(l->state != r->state)
    return 0;
  switch(l->state){
  case 0: return l->val.scm_int == r->val.scm_int;
  case 1: return !strcmp(l->val.scm_sym, r->val.scm_sym);
  case 2: case 3: return l == r;
  }
  return 0;
}
scm_t scm_eq(scm_t l, scm_t r){
  return mkInt(scm_eq_raw(l, r));
}

scm_t display(scm_t s){
  switch(s->state){
  case 0: printf("%d\n", s->val.scm_int); break;
  case 1: printf("%s\n", s->val.scm_sym); break;
  case 2: case 3: printf("<<opaque type>>\n");
  }
  return s;
}

void scm_apply(int i, scm_t f, ...) __attribute__((noreturn)) {
  int x;
  va_list va;
  scm_t *arg_list = malloc(sizeof(scm_t) * i + 1);
  va_start(va, f);
  for(x = 1; x < i+1; ++x){
    arg_list[x] = va_arg(va, scm_t);
  }
  if(f->state != 4){
    printf("Attempted to apply nonfunction\n");
    exit(1);
  } else {
    arg_list[0] = f->val.scm_lam.clos;
    f->val.scm_lam.fun(arg_list);
  }
}

scm_t scm_plus(scm_t l, scm_t r){
  if(l->state || r->state){
    printf("Attempted to add non-numbers\n");
    exit(1);
  }
  return mkInt(l->val.scm_int + r->val.scm_int);
}
scm_t scm_sub(scm_t l, scm_t r){
  if(l->state || r->state){
    printf("Attempted to subtract non-numbers\n");
    exit(1);
 }
  return mkInt(l->val.scm_int - r->val.scm_int);
}

scm_t scm_mult(scm_t l, scm_t r){
  if(l->state || r->state){
    printf("Attempted to multiply non-numbers\n");
    exit(1);
  }
  return mkInt(l->val.scm_int * r->val.scm_int);
}

scm_t scm_div(scm_t l, scm_t r){
  if(l->state || r->state){
    printf("Attempted to divide non-numbers\n");
    exit(1);
  }
  return mkInt(l->val.scm_int / r->val.scm_int);
}


scm_t scm_cons(scm_t h, scm_t t){
  scm_t scm_s = scm_malloc();
  struct scheme_val s = {.state = 2,
                         {.scm_cons = {.head = h, .tail = t}}};
  memcpy(scm_s, &s, sizeof *scm_s);
  return scm_s;
}


scm_t scm_car(scm_t s){
  if(s->state != 2){
    printf("Attempted to car non-pair\n");
    exit(1);
  }
  return s->val.scm_cons.head;
}
scm_t scm_cdr(scm_t s){
  if(s->state != 2){
    printf("Attempted to cdr non-pair\n");
    exit(1);
  }
  return s->val.scm_cons.tail;
}

void scm_halt(scm_t l) __attribute__((noreturn)) {exit(0)}

scm_t scm_select_clos(int ind, scm_t clos){
  if(clos->state != 3){
    printf("Attempted to select with non-closure\n");
    exit(1);
  }
  return clos->val.scm_clos.closed[ind];
}
scm_t scm_write_clos(scm_t ind, scm_t val, scm_t clos){
  if(ind->state || clos->state != 3){
    printf("Write to select with non-closure or integer\n");
    exit(1);
  }
  return clos->val.scm_clos.closed[ind->val.scm_int] = val;
}

scm_t scm_top_clos = NULL;

