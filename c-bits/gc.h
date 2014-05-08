#ifndef GC
#define GC
#include "types.h"
void gc_init();
void mark(scm_t);
void sweep();
void register_closure(scm_t);
#endif
