#include "rts.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h> // Weeeee
#include <glib.h>  // For hashtables

// Global variables for preserving
// continuations during longjmp's
static scm_t  current_fun;
static scm_t* current_args;
static jmp_buf env;

// Global counter of function calls
static int stack_frames;

// A global registery of allocated closures, needed
// for GC
GHashTable* live_closures;

// Types
struct scheme_val;
typedef struct clos {
  struct scheme_val** closed;
  int length;
  int live; // Used by mark-and-sweep GC
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
  struct scheme_val s = {.state = 3, {.scm_clos = {.closed = NULL, .length = i, .live = 0}}};

  va_start(ap, i);
  for(x = 0; x < i; ++x){
    closed[x] = va_arg(ap, scm_t);
  }
  va_end(ap);
  s.val.scm_clos.closed = closed;
  memcpy(result, &s, sizeof *result);
  g_hash_table_add(live_closures, result);
  return result;
}

scm_t mkLam(scm_t c, lam_t l){
  scm_t scm_s = scm_malloc();
  struct scheme_val s = {.state = 4, {.scm_lam = {c, l}}};
  memcpy(scm_s, &s, sizeof *scm_s);
  return scm_s;
}

void free_scm_t(scm_t t){
  free(t);
  t = NULL; // Leaks subchildren
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

void mark(scm_t root){
  int i;
  scm_t obj;
  if(!root) return; // Watch out for top_clos

  if(root->state != 3){
    return;
  }
  root->val.scm_clos.live = 1;

  for(i = 0; i < root->val.scm_clos.length; ++i){
    obj = root->val.scm_clos.closed[i];
    if(!obj) continue; // Watch out for top_clos again
    if(obj->state != 3 && obj->state != 4) continue; // Don't care about non-closures
    if(obj->state == 3){
      if(!obj->val.scm_clos.live){
        mark(obj); // DFS on closures
      }
    } else {
      if(!obj->val.scm_lam.clos->val.scm_clos.live){
        mark(obj->val.scm_lam.clos); // Use lambda's closures too since we need to keep
      }
    }
  }
}

void sweep(){
  int live;
  unsigned int length, i;
  scm_t obj;
  
  GList* keys = g_hash_table_get_keys(live_closures);
  while(keys){
    obj = keys->data;
    if(!obj) continue;
    live = ((scm_t) obj)->val.scm_clos.live;
    if(!live){ // If unmarked, sweep
      g_hash_table_remove(live_closures, obj);
      free_scm_t(obj);
    } else {
      ((scm_t) obj)->val.scm_clos.live = 0;
    }
    keys = keys->next;
  }
}


void scm_init(lam_t f){
  live_closures = g_hash_table_new(NULL, NULL); // Setup the hashtable, NULL and NULL 
                                                // directs glib to use simple hash & eq functions
  stack_frames = 0;

  if(setjmp(env)){
    printf("Jumped\n");
    stack_frames = 0;
    // Do GC
    mark(current_args[0]); // The root is the current closure which is always the first arg
    sweep();
    // Call next continuation
    current_fun->val.scm_lam.fun(current_args);
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
      current_fun     = f;
      current_args    = arg_list;
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

