#ifdef DEVBUILD

#include "libs7.h"

extern s7_scheme *s7;

/* HACK: need these structs from mustach-wrap and mustach-libs7
   so we can deref void* closure in mustach.c
*/
/* struct Xwrap { */
/*     int predicate; /\* so mustach.c can signal that metatag is '?' *\/ */
/* 	/\* original interface *\/ */
/* 	const struct mustach_wrap_itf *itf; */

/* 	/\* original closure *\/ */
/* 	void *closure; */

/* 	/\* flags *\/ */
/* 	int flags; */

/* 	/\* emiter callback *\/ */
/* 	void /\* mustach_emit_cb_t *\/ *emitcb; */

/* 	/\* write callback *\/ */
/* 	void /\* mustach_write_cb_t *\/ *writecb; */
/* }; */

/* /\* must match struct expl in mustach-libs7.c *\/ */
/* struct Xexpl { */
/*     int predicate; */
/*     bool lambda; */
/*     s7_pointer root; */
/*     s7_pointer selection;       /\* ? *\/ */
/*     int depth; */
/*     struct { */
/*         s7_pointer cont;        /\* context? containing object? *\/ */
/*         s7_pointer obj;         /\* current elt of ctx?  *\/ */
/*         s7_pointer iter; */
/*         int is_objiter;         /\* obj type has iterators *\/ */
/*         size_t index, count;    /\* current idx, sibling count *\/ */
/*         int predicate;            /\*  *\/ */
/*         bool lambda;            /\*  *\/ */
/*         int  workbuf_idx; */
/*     } stack[MUSTACH_MAX_DEPTH]; */
/* }; */

struct metastack_s {
    const char *key; /* tagkey */
    size_t keylen;
    const char *section_content_start;  /* template ptr, set at section begin, used to control iteration in sections (was: 'again') */
    unsigned enabled:   1; /* bitfield */
    unsigned entered:   1; /* bitfield */
    unsigned lambda:    1; /* bitfield */
    int predicate;
    signed int workbuf_idx; /* -1 if hashtag is not a lambda; index into workbuf_stack */
};

/* void dump_stack(struct Xexpl *closure); */
void dump_metastack(int idx, void *ms);

#define DUMP(msg, x) (({char*s=s7_object_to_c_string(s7, x);log_debug("%s: %s", msg, s);fflush(NULL);free(s);}))

/* #define DUMP_CLOSURE(x, i) */
#define DUMP_CLOSURE(x, i) (({ \
                char*s=s7_object_to_c_string(s7, x->root);    \
                log_debug("e->root %s", s); \
                free(s); \
                s=s7_object_to_c_string(s7, x->selection); \
                log_debug("e->selection %s", s); \
                free(s); \
                log_debug("e->depth %d", x->depth); \
                if (x->stack[i].obj) {                        \
                    s=s7_object_to_c_string(s7, x->stack[i].obj);       \
                    log_debug("e->stack[%d].obj: %s", i, s);            \
                    free(s);                                            \
                } else {                                                \
                    log_debug("e->stack[%d].obj: NULL", i);             \
                }                                                       \
                if (x->stack[i].cont) {                                 \
                    s=s7_object_to_c_string(s7, x->stack[i].cont);      \
                    log_debug("e->stack[%d].cont: %s", i, s);           \
                    free(s);                                            \
                } else {                                                \
                    log_debug("e->stack[%d].cont: NULL", i);            \
                }                                                       \
                log_debug("e->stack[%d].count: %d", i, x->stack[i].count); \
                log_debug("e->stack[%d].index: %d", i, x->stack[i].index); \
                log_debug("e->stack[%d].lambda: %d", i, x->stack[i].lambda); \
                log_debug("e->stack[%d].predicate: %d", i, x->stack[i].predicate); \
                log_debug("e->stack[%d].workbuf_idx: %d", i, x->stack[i].workbuf_idx); \
                s7_flush_output_port(s7, s7_current_output_port(s7));   \
                s7_flush_output_port(s7, s7_current_error_port(s7)); \
                fflush(NULL);}))
#else
#define DUMP(msg,x)
#define DUMP_CLOSURE(x)
#endif
