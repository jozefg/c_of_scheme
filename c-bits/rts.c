#include "rts.h"
#include "gc.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h> // Weeeee

// Global variables for preserving
// continuations during longjmp's
static scm_t  current_fun;
static scm_t* current_args;
static int    current_args_len;
static int    jump_num;
static jmp_buf env;

// Global counter of function calls
static int stack_frames;

scm_t scm_malloc(){
  return malloc(sizeof(struct scheme_val));
  /* Pretend there's a cool GC here */
}

scm_t mkInt(int i){
  scm_t scm_i = scm_malloc();
  struct scheme_val s = {.live = 0, .state = 0, {.scm_int = i}};
  memcpy(scm_i, &s, sizeof *scm_i);
  return scm_i;
}

scm_t mkSym(char *c){
  scm_t scm_s = scm_malloc();
  struct scheme_val s = {.live = 0, .state = 1, {.scm_sym = c}};
  memcpy(scm_s, &s, sizeof *scm_s);
  return scm_s;
}

scm_t mkClos(int i, ...){
  va_list ap;
  int x;
  scm_t result = scm_malloc();
  scm_t *closed = malloc(sizeof(scm_t) * i);
  struct scheme_val s = {.live = 0, .state = 3, {.scm_clos = {.closed = NULL, .length = i}}};

  va_start(ap, i);
  for(x = 0; x < i; ++x){
    closed[x] = va_arg(ap, scm_t);
  }
  va_end(ap);
  s.val.scm_clos.closed = closed;
  memcpy(result, &s, sizeof *result);
  register_closure(result);
  return result;
}

scm_t mkLam(scm_t c, lam_t l){
  scm_t scm_s = scm_malloc();
  struct scheme_val s = {.live = 0, .state = 4, {.scm_lam = {c, l}}};
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

void scm_init(lam_t f){
  int i;
  stack_frames = 0;
  jump_num     = 0;
  gc_init();
  
  if(setjmp(env)){
    stack_frames = 0;
    ++jump_num;

    if(jump_num == 1000){ // Do GC on 100th 200th.. jumps
      jump_num = 0;
      for(i = 0; i < current_args_len; ++i){ // Do GC
        mark(current_args[i]);
      }
      sweep();
    }
    current_fun->val.scm_lam.fun(current_args); // Call next continuation
  }

  scm_apply(0, mkLam(scm_top_clos, f)); // Call main
}

void scm_apply(int i, scm_t f, ...) {
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

    if(stack_frames >= 100){
      // Transfer continuation up
      current_fun      = f;
      current_args     = arg_list;
      current_args_len = i + 1;
      longjmp(env, 1);
    }
    ++stack_frames;
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
  struct scheme_val s = {.live = 0, .state = 2,
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

void scm_halt(scm_t l) {exit(0);}
scm_t scm_stop(){
  exit(0);
  return NULL;
}

scm_t scm_select_clos(int ind, scm_t clos){
  if(clos->state != 3){
    printf("Attempted to select with non-closure\n");
    printf("%d\n", clos->state);
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

