#ifndef GC
#define GC
void gc_init();
void mark(scm_t);
void sweep();
void register_closure(scm_t);
#endif
