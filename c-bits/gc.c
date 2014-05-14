#include "gc.h"
#include <stdlib.h>
#include <stdio.h>
#include <glib.h>  // For hashtables and queues

// A global registery of allocated closures, needed for GC
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
  GQueue* queue = g_queue_new();
  if(!root || root->state != 3) return; // Watch out for top_clos and friends

  root->live = 1; // Setup the queue with root
  g_queue_push_head(queue, root);

  while(g_queue_get_length(queue) && (root = g_queue_pop_head(queue))){
    for(i = 0; i < root->val.scm_clos.length; ++i){ // For each object in the closure
      obj = root->val.scm_clos.closed[i];
      if(!obj) continue; // top_clos
      if (obj->state != 3 && obj->state != 4){
        obj->live = 1;
        continue; // Don't care about non-closures
      }
      // Unwrap a lam clos since we may need to call it in future
      if(obj->state == 4) obj = obj->val.scm_lam.clos;
      
      if(obj && !obj->live){ // Extra obj check in case lambda is boxed with bad closure
        obj->live = 1;
        g_queue_push_head(queue, obj); // DFS on closures
      }
    }
  }
  g_queue_free(queue);
}

void mark(scm_t t){
  if(!t) return;
  switch(t->state){
  case 0: case 1: t->live=1; break;
  case 2:
    mark(t->val.scm_cons.head);
    mark(t->val.scm_cons.tail);
    break;
  case 3:
    mark_clos(t);
    break;
  case 4:
    mark_clos(t->val.scm_lam.clos);
    break;
  }
}
      
void free_scm_t(scm_t t){
  free(t); // Leaks subchildren
}

void sweep(){
  scm_t obj;
  printf("Sweeping\n");
  GList* keys = g_hash_table_get_keys(live_closures);
  while(keys){
    obj = keys->data;
    if(!obj) continue;
    if(!((scm_t) obj)->live){ // If unmarked, sweep
      g_hash_table_remove(live_closures, obj);
      free_scm_t(obj);
    } else { // Reset marked closures for next run
      ((scm_t) obj)->live = 0;
    }
    keys = keys->next;
  }
}
