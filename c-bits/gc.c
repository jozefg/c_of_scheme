#include "rts.h"
#include "gc.h"
#include <glib.h>  // For hashtables

// A global registery of allocated closures, needed
// for GC
static GHashTable* live_closures;

void gc_init(){
  live_closures = g_hash_table_new(NULL, NULL);
}

void register_closure(scm_t clos){
  g_hash_table_add(live_closures, clos);
}

void mark_clos(scm_t root){
  int i;
  scm_t obj;
  if(!root || root->state != 3) return; // Watch out for top_clos and friends

  root->val.scm_clos.live = 1;

  for(i = 0; i < root->val.scm_clos.length; ++i){
    obj = root->val.scm_clos.closed[i];
    if(!obj) continue; // Watch out for top_clos again
    if(obj->state != 3 && obj->state != 4) continue; // Don't care about non-closures
    if(obj->state == 3){
      if(!obj->val.scm_clos.live){
        mark_clos(obj); // DFS on closures
      }
    } else {
      if(obj->val.scm_lam.clos && !obj->val.scm_lam.clos->val.scm_clos.live){
        mark_clos(obj->val.scm_lam.clos); // Use lambda's closures too since we need to keep
      }
    }
  }
}

void mark(scm_t t){
  if(!t) return;
  switch(t->state){
  case 0:
  case 1: break;
  case 2:
    mark(t->val.scm_cons.head);
    mark(t->val.scm_cons.tail);
  case 3:
    mark_clos(t);
  case 4:
    mark_clos(t->val.scm_lam.clos);
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
