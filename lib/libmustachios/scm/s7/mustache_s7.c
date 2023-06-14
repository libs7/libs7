/* scheme to json util: https://github.com/joeltg/json.scm */
/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"
#include "log.h"
#include "mustach.h"
#include "mustache_s7.h"

/* #ifdef DEVBUILD */
/* #include "ansi_colors.h" */
/* #include "debug.h" */
/* #endif */

/* We need a global s7, because these routines are called by the
   mustach kernel, which knows nothing about s7.
   The definition must be set by client.
*/
extern s7_scheme *s7;

static void dump_stack(struct tstack_s *stack);

/* one workbuf per lambda #tag, to accumulate content */
int workbufs_tos = 0;               /* top of stack */
struct workbuf_s {
    char buf[4096]; /* big enough to hold (resolved) content between {{#f}} and {{/f}} */
} workbuf_stack[16]; /* nesting - content of lambda tag could contain other lambda tags
                       e.g. {{#f}} ffoo {{#g}} gfoo {{#h}} hfoo {{hitem}} hbar {{/h}} gbar {{/g}} fbar {{/f}}
                       any of f, g, h, and x could be a lambda. #h could contain multiple htimems.
                       so the stack of buffers may not match the stack of hashtags one-to-one.
                     */

static int start(struct tstack_s *stack)
{
    TRACE_ENTRY(start)
    stack->depth = 0;
    s7_pointer x = s7_nil(s7);
    stack->selection = x;
    stack->stack[0].ctx = s7_nil(s7);
    stack->stack[0].obj = stack->root;
    stack->stack[0].index = 0;
    stack->stack[0].count = 1;
    fflush(NULL);
    return MUSTACH_OK;
}

static int compare(void *closure, const char *value)
{
#ifdef DEVBUILD
    log_debug("compare");
#endif
    struct tstack_s *e = closure;
    s7_pointer o = e->selection;

    if (s7_is_real(o)) {
        s7_double d = s7_number_to_real(s7, o) - atof(value);
        return d < 0 ? -1 : d > 0 ? 1 : 0;
    } else if (s7_is_integer(o)) {
        s7_int i = s7_number_to_integer(s7, o) - atoll(value);
        return i < 0 ? -1 : i > 0 ? 1 : 0;
    } else if (s7_is_string(o)) {
        const char *s = s7_string(o);
        return strcmp(s, value);
    } else if (o == s7_t(s7)) {
        return strcmp("true", value);
    } else if (o == s7_f(s7)) {
        return strcmp("false", value);
    } else if (o == s7_nil(s7)) {
        return strcmp("null", value);
    } else {
        return 1;
    }
}

static s7_pointer _handle_stack_predicate(struct tstack_s *e, s7_pointer assoc)
{
    s7_pointer selection;
    s7_pointer assoc_val = s7_cdr(assoc);
    TRACE_ENTRY(_handle_stack_predicate);
    e->stack[e->depth].predicate = e->predicate;
    // check if assoc is procedure
    if (s7_is_proper_list(s7, assoc)) {
        // may be UNDOTTED list of values or a proc
        // vals: '((:nbrs (1 2 3))), '((:name Bob))
        // WARNING: (:nbrs . (1 2 3) == (:nbrs 1 2 3)
        // proc: `((:world-thunk ,(lambda () ... )))
        // NB: quasiquote and unquote, so the
        // lambda will be evaluated
        // cdr is (1 2 3) or (Bob) or (#<lambda ()>)
        // cadr is 1 or Bob or #<lambda ()>
        s7_pointer cadr = s7_cadr(assoc);
        TRACE_S7_DUMP("cadr", cadr);
        if (s7_is_procedure(cadr)) {
            if (e->stack[e->depth].index +1 < e->stack[e->depth].count) {
                e->lambda = true;
                e->stack[e->depth].lambda = true;
            } else {
                e->lambda = false;
                e->stack[e->depth].lambda = false;
            }
            e->stack[e->depth].lambda = true;
#ifdef DEVBUILD
            log_debug("is_proc: %d", s7_is_procedure(cadr));
#endif
            selection = s7_f(s7); /* FIXME: support lambda predicates */
        } else {
            if (e->stack[e->depth].index + 1 == e->stack[e->depth].count) {
                selection = s7_f(s7);
            } else {
                s7_pointer ctx = e->stack[e->depth].ctx;
#ifdef DEVBUILD
                TRACE_S7_DUMP("last in ctx?", ctx);
                log_debug("index: %d", e->stack[e->depth].index);
#endif
                // search rest of ctx to see if current key is last
                // I'm sure there is a more efficient way to do this...
                s7_pointer tail;
                if (s7_is_vector(ctx)) {
#ifdef DEVBUILD
                    log_debug("ctx is vector");
#endif
                    s7_pointer ls = s7_vector_to_list(s7, ctx);
                    TRACE_S7_DUMP("list from vec", ls);
                    tail = s7_call(s7, s7_name_to_value(s7, "drop"),
                                              s7_list(s7, 2, ls,
                                                      s7_make_integer(s7, e->stack[e->depth].index + 1)));
                    TRACE_S7_DUMP("vec nthcdr", tail);
                    s7_pointer key = s7_car(assoc);
                    TRACE_S7_DUMP("key", key);
                    s7_pointer env = s7_inlet(s7,
                                              s7_list(s7, 2,
                                                      s7_cons(s7,
                                                              s7_make_symbol(s7, "tail"),
                                                              tail),
                                                      s7_cons(s7,
                                                              s7_make_symbol(s7, "key"),
                                                              key)));
                    char *find = "(find-if (lambda (alist) (assoc key alist)) tail)";
                    s7_pointer found = s7_eval_c_string_with_environment(s7, find, env);
                    TRACE_S7_DUMP("found", found);
                    if (found == s7_f(s7))
                        selection = s7_f(s7);
                    else
                        selection = s7_t(s7);
                } else if (s7_is_list(s7, ctx)) {
                    // s7_pointer drop = s7_name_to_value(s7, "drop");
                    s7_pointer tail = s7_call(s7, s7_name_to_value(s7, "drop"),
                                              s7_list(s7, 2, ctx,
                                                      s7_make_integer(s7, e->stack[e->depth].index)));
                    TRACE_S7_DUMP("list nthcdr", tail);
                    (void)tail;
                    selection = s7_f(s7);
                } else {
                    selection = s7_f(s7);
                }
            }
        }
    }
    else if (s7_is_list(s7, assoc)) {
        // DOTTED list, may be values or a proc
        TRACE_S7_DUMP("is_list", assoc);
        if (s7_is_procedure(assoc_val)) {
            selection = assoc_val;
            e->stack[e->depth].lambda = true;
            if (s7_is_symbol(assoc_val)) {
                s7_pointer x = s7_symbol_value(s7, assoc_val);
                TRACE_S7_DUMP("x", x);
                log_debug("proc? %d", s7_is_procedure(x));
                selection = s7_f(s7); /* FIXME: support lambda predicates */
            }
        } else {
            e->stack[e->depth].predicate = e->predicate;
            if (e->stack[e->depth].index + 1 == e->stack[e->depth].count) {
                selection = s7_f(s7);
            } else {
                s7_pointer ctx = e->stack[e->depth].ctx;
#ifdef DEVBUILD
                TRACE_S7_DUMP("last in ctx?", ctx);
                log_debug("index: %d", e->stack[e->depth].index);
#endif
                // search rest of ctx to see if current key is last
                // I'm sure there is a more efficient way to do this...
                s7_pointer tail;
                if (s7_is_vector(ctx)) {
#ifdef DEVBUILD
                    log_debug("ctx is vector");
#endif
                    s7_pointer ls = s7_vector_to_list(s7, ctx);
                    TRACE_S7_DUMP("list from vec", ls);
                    tail = s7_call(s7, s7_name_to_value(s7, "drop"),
                                              s7_list(s7, 2, ls,
                                                      s7_make_integer(s7, e->stack[e->depth].index + 1)));
                    TRACE_S7_DUMP("vec nthcdr", tail);
                    s7_pointer key = s7_car(assoc);
                    TRACE_S7_DUMP("key", key);
                    s7_pointer env = s7_inlet(s7,
                                              s7_list(s7, 2,
                                                      s7_cons(s7,
                                                              s7_make_symbol(s7, "tail"),
                                                              tail),
                                                      s7_cons(s7,
                                                              s7_make_symbol(s7, "key"),
                                                              key)));
                    char *find = "(find-if (lambda (alist) (assoc key alist)) tail)";
                    s7_pointer found = s7_eval_c_string_with_environment(s7, find, env);
                    TRACE_S7_DUMP("found", found);
                    if (found == s7_f(s7))
                        selection = s7_f(s7);
                    else
                        selection = s7_t(s7);
                } else if (s7_is_list(s7, ctx)) {
                    // s7_pointer drop = s7_name_to_value(s7, "drop");
                    s7_pointer tail = s7_call(s7, s7_name_to_value(s7, "drop"),
                                              s7_list(s7, 2, ctx,
                                                      s7_make_integer(s7, e->stack[e->depth].index)));
                    TRACE_S7_DUMP("list nthcdr", tail);
                    (void)tail;
                    selection = s7_f(s7);
                } else {
                    selection = s7_f(s7);
                }
            }
            /* } else { */
            /*     selection = assoc_val; */
        }
    }
    else {
        // not a list
        if (e->stack[e->depth].index + 1 == e->stack[e->depth].count) {
            selection = s7_f(s7);
        } else {
#ifdef DEVBUILD
            log_debug("pred satisfied");
#endif
            selection = s7_t(s7);
        }
    }
    return selection;
}

/* static s7_pointer _handle_stack_predicate_ht(struct tstack_s *e, s7_pointer entry) */
/* { */
/*     (void)e; */
/*     return entry; */
/* } */
/* { */
/*     s7_pointer selection; */
/*     s7_pointer assoc_val = s7_cdr(entry); */
/*     TRACE_ENTRY(_handle_stack_predicate); */
/*     e->stack[e->depth].predicate = e->predicate; */
/*     // check if assoc is procedure */
/*     if (s7_is_proper_list(s7, entry)) { */
/*         // may be UNDOTTED list of values or a proc */
/*         // vals: '((:nbrs (1 2 3))), '((:name Bob)) */
/*         // WARNING: (:nbrs . (1 2 3) == (:nbrs 1 2 3) */
/*         // proc: `((:world-thunk ,(lambda () ... ))) */
/*         // NB: quasiquote and unquote, so the */
/*         // lambda will be evaluated */
/*         // cdr is (1 2 3) or (Bob) or (#<lambda ()>) */
/*         // cadr is 1 or Bob or #<lambda ()> */
/*         s7_pointer cadr = s7_cadr(entry); */
/*         TRACE_S7_DUMP("cadr", cadr); */
/*         if (s7_is_procedure(cadr)) { */
/*             if (e->stack[e->depth].index +1 < e->stack[e->depth].count) { */
/*                 e->lambda = true; */
/*                 e->stack[e->depth].lambda = true; */
/*             } else { */
/*                 e->lambda = false; */
/*                 e->stack[e->depth].lambda = false; */
/*             } */
/*             e->stack[e->depth].lambda = true; */
/* #ifdef DEVBUILD */
/*             log_debug("is_proc: %d", s7_is_procedure(cadr)); */
/* #endif */
/*             selection = s7_f(s7); /\* FIXME: support lambda predicates *\/ */
/*         } else { */
/*             if (e->stack[e->depth].index + 1 == e->stack[e->depth].count) { */
/*                 selection = s7_f(s7); */
/*             } else { */
/*                 s7_pointer ctx = e->stack[e->depth].ctx; */
/* #ifdef DEVBUILD */
/*                 TRACE_S7_DUMP("last in ctx?", ctx); */
/*                 log_debug("index: %d", e->stack[e->depth].index); */
/* #endif */
/*                 // search rest of ctx to see if current key is last */
/*                 // I'm sure there is a more efficient way to do this... */
/*                 s7_pointer tail; */
/*                 if (s7_is_vector(ctx)) { */
/* #ifdef DEVBUILD */
/*                     log_debug("ctx is vector"); */
/* #endif */
/*                     s7_pointer ls = s7_vector_to_list(s7, ctx); */
/*                     TRACE_S7_DUMP("list from vec", ls); */
/*                     tail = s7_call(s7, s7_name_to_value(s7, "drop"), */
/*                                               s7_list(s7, 2, ls, */
/*                                                       s7_make_integer(s7, e->stack[e->depth].index + 1))); */
/*                     TRACE_S7_DUMP("vec nthcdr", tail); */
/*                     s7_pointer key = s7_car(entry); */
/*                     TRACE_S7_DUMP("key", key); */
/*                     s7_pointer env = s7_inlet(s7, */
/*                                               s7_list(s7, 2, */
/*                                                       s7_cons(s7, */
/*                                                               s7_make_symbol(s7, "tail"), */
/*                                                               tail), */
/*                                                       s7_cons(s7, */
/*                                                               s7_make_symbol(s7, "key"), */
/*                                                               key))); */
/*                     char *find = "(find-if (lambda (alist) (assoc key alist)) tail)"; */
/*                     s7_pointer found = s7_eval_c_string_with_environment(s7, find, env); */
/*                     TRACE_S7_DUMP("found", found); */
/*                     if (found == s7_f(s7)) */
/*                         selection = s7_f(s7); */
/*                     else */
/*                         selection = s7_t(s7); */
/*                 } else if (s7_is_list(s7, ctx)) { */
/*                     // s7_pointer drop = s7_name_to_value(s7, "drop"); */
/*                     s7_pointer tail = s7_call(s7, s7_name_to_value(s7, "drop"), */
/*                                               s7_list(s7, 2, ctx, */
/*                                                       s7_make_integer(s7, e->stack[e->depth].index))); */
/*                     TRACE_S7_DUMP("list nthcdr", tail); */
/*                     (void)tail; */
/*                     selection = s7_f(s7); */
/*                 } else { */
/*                     selection = s7_f(s7); */
/*                 } */
/*             } */
/*         } */
/*     } */
/*     else if (s7_is_list(s7, assoc)) { */
/*         // DOTTED list, may be values or a proc */
/*         TRACE_S7_DUMP("is_list", assoc); */
/*         if (s7_is_procedure(assoc_val)) { */
/*             selection = assoc_val; */
/*             e->stack[e->depth].lambda = true; */
/*             if (s7_is_symbol(assoc_val)) { */
/*                 s7_pointer x = s7_symbol_value(s7, assoc_val); */
/*                 TRACE_S7_DUMP("x", x); */
/*                 log_debug("proc? %d", s7_is_procedure(x)); */
/*                 selection = s7_f(s7); /\* FIXME: support lambda predicates *\/ */
/*             } */
/*         } else { */
/*             e->stack[e->depth].predicate = e->predicate; */
/*             if (e->stack[e->depth].index + 1 == e->stack[e->depth].count) { */
/*                 selection = s7_f(s7); */
/*             } else { */
/*                 s7_pointer ctx = e->stack[e->depth].ctx; */
/* #ifdef DEVBUILD */
/*                 TRACE_S7_DUMP("last in ctx?", ctx); */
/*                 log_debug("index: %d", e->stack[e->depth].index); */
/* #endif */
/*                 // search rest of ctx to see if current key is last */
/*                 // I'm sure there is a more efficient way to do this... */
/*                 s7_pointer tail; */
/*                 if (s7_is_vector(ctx)) { */
/* #ifdef DEVBUILD */
/*                     log_debug("ctx is vector"); */
/* #endif */
/*                     s7_pointer ls = s7_vector_to_list(s7, ctx); */
/*                     TRACE_S7_DUMP("list from vec", ls); */
/*                     tail = s7_call(s7, s7_name_to_value(s7, "drop"), */
/*                                               s7_list(s7, 2, ls, */
/*                                                       s7_make_integer(s7, e->stack[e->depth].index + 1))); */
/*                     TRACE_S7_DUMP("vec nthcdr", tail); */
/*                     s7_pointer key = s7_car(assoc); */
/*                     TRACE_S7_DUMP("key", key); */
/*                     s7_pointer env = s7_inlet(s7, */
/*                                               s7_list(s7, 2, */
/*                                                       s7_cons(s7, */
/*                                                               s7_make_symbol(s7, "tail"), */
/*                                                               tail), */
/*                                                       s7_cons(s7, */
/*                                                               s7_make_symbol(s7, "key"), */
/*                                                               key))); */
/*                     char *find = "(find-if (lambda (alist) (assoc key alist)) tail)"; */
/*                     s7_pointer found = s7_eval_c_string_with_environment(s7, find, env); */
/*                     TRACE_S7_DUMP("found", found); */
/*                     if (found == s7_f(s7)) */
/*                         selection = s7_f(s7); */
/*                     else */
/*                         selection = s7_t(s7); */
/*                 } else if (s7_is_list(s7, ctx)) { */
/*                     // s7_pointer drop = s7_name_to_value(s7, "drop"); */
/*                     s7_pointer tail = s7_call(s7, s7_name_to_value(s7, "drop"), */
/*                                               s7_list(s7, 2, ctx, */
/*                                                       s7_make_integer(s7, e->stack[e->depth].index))); */
/*                     TRACE_S7_DUMP("list nthcdr", tail); */
/*                     (void)tail; */
/*                     selection = s7_f(s7); */
/*                 } else { */
/*                     selection = s7_f(s7); */
/*                 } */
/*             } */
/*             /\* } else { *\/ */
/*             /\*     selection = assoc_val; *\/ */
/*         } */
/*     } */
/*     else { */
/*         // not a list */
/*         if (e->stack[e->depth].index + 1 == e->stack[e->depth].count) { */
/*             selection = s7_f(s7); */
/*         } else { */
/* #ifdef DEVBUILD */
/*             log_debug("pred satisfied"); */
/* #endif */
/*             selection = s7_t(s7); */
/*         } */
/*     } */
/*     return selection; */
/* } */

static s7_pointer _handle_stack_match(struct tstack_s *e, s7_pointer assoc)
{
    TRACE_ENTRY(_handle_stack_match);
    s7_pointer selection;
    s7_pointer assoc_val = s7_cdr(assoc);
    if (s7_is_proper_list(s7, assoc)) {
        // first check for proper (undotted) lists
        // (proper-list? '(:a . b)) => #f
        // (proper-list? '(:a 1) => #t
        // (proper-list? '(:a (1)) => #t
        // (proper-list? '(:a 1 2 3)) => #t
        // (proper-list? '(:a . (1 2 3))) => #t
        // NB: (:a . (1 2 3)) evaluates to (:a 1 2 3))
        // (proper-list? '(:a (1 2 3))) => #t
        // (proper-list? . ,(:lambda ...)) => #f
        // (proper-list? ,(:lambda ...)) => #t
        TRACE_S7_DUMP("assoc is " RED " proper list" CRESET, assoc);
        TRACE_S7_DUMP("assoc-val", assoc_val);

        // special handling:
        // (:a 1 2 3),  (:a (1 2 3)), (:a (:b 1)), (:a ((:b 1)))
        // cdr always a propert list
        // (cdr (:a 1 2 3)) > (1 2 3)
        // (cdr (:a (1 2 3))) > ((1 2 3))
        // in latter case, (cddr x) = '()
        // but: (:a (1 2) (3 4)) - list of (a)lists
        // then val is ((1 2) (3 4))
        // pathological: (:a (:b 1) (:c 2) 99)
        //   val is  ((:b 1) (:c 2) 99) - not an alist

        selection = assoc_val;

        //OBSOLETE: unwrapping implicit lists. implicit lists are retained.

/* #ifdef DEVBUILD */
/*         log_debug("assoc-val len: %d", s7_list_length(s7, assoc_val)); */
/* #endif */
/*         if (s7_list_length(s7, assoc_val) == 1) { */
/*             // unwrap singleton alist (((:a ...))) */
/*             // case: (1)  from (:a 1) */
/*             s7_pointer car = s7_car(assoc_val); */
/*             if (s7_is_list(s7, car)) { */
/*                 // case: ((1)) */
/*                 // case: ((1 2 3)) */
/*                 // case: ((lambda ...)) */
/*                 TRACE_S7_DUMP("car is list", car); */
/*                 if (car == s7_nil(s7)) { */
/*                     selection = car; */
/*                 } */
/*                 else if (libs7_is_alist(s7, car)) { */
/*                     TRACE_S7_DUMP("car is alist", car); */
/*                     selection = car; */
/*                 } else { */
/*                     selection = assoc_val; */
/*                 } */
/*             } else { */
/*                 TRACE_S7_DUMP("car is NOT list", car); */
/*                 if (s7_is_procedure(car)) { */
/*                     TRACE_LOG_DEBUG("car is lambda", ""); */
/*                     if (e->stack[e->depth].index +1 < e->stack[e->depth].count) { */
/*                         e->lambda = true; */
/*                     } else { */
/*                         e->lambda = false; */
/*                         e->stack[e->depth].lambda = false; */
/*                     } */
/*                     e->stack[e->depth].lambda = true; */
/*                     selection = car; */
/*                 } */
/*                 else if (s7_is_vector(car)) { */
/* #ifdef DEVBUILD */
/*                     log_debug("car is vector"); */
/* #endif */
/*                     selection = car; */
/*                 } else { */
/* #ifdef DEVBUILD */
/*                     log_debug("car is nonlambda, nonvector"); */
/* #endif */
/*                     selection = car; */
/*                 } */
/*             } */
/*         } else { */
/* #ifdef DEVBUILD */
/*             log_debug("assoc_val len > 1"); */
/* #endif */
/*             selection = assoc_val; */
/*         } */
    }
    else {
        // improper list, cdr is never a list
        /* else if (s7_is_list(s7, assoc)) { */
        // DOTTED list, may be values or a proc
        TRACE_S7_DUMP("assoc is " RED "improper list" CRESET, assoc);
        TRACE_S7_DUMP("assoc-val", assoc_val);
        // assoc_val never a list
        if (s7_is_procedure(assoc_val)) {
            TRACE_LOG_DEBUG("is lambda", "");
            e->stack[e->depth].lambda = true;
            selection = assoc_val;
            /* if (s7_is_symbol(assoc_val)) { */
            /*     s7_pointer x = s7_symbol_value(s7, assoc_val); */
            /*     TRACE_S7_DUMP("x", x); */
            /*     log_debug("proc? %d", s7_is_procedure(x)); */
            /*     selection = x; */
            /* } */
            /* selection = s7_apply_function(s7, */
            /*                       assoc_val, */
            /*                       s7_nil(s7)); */
        } else {
#ifdef DEVBUILD
            log_debug("not lambda");
#endif
            selection = assoc_val;
        }
    }
    return selection;
}

/* static s7_pointer _handle_stack_match_ht(struct tstack_s *e, s7_pointer assoc) */
/* { */
/*     TRACE_ENTRY(_handle_stack_match_ht); */
/*     s7_pointer selection; */
/*     s7_pointer assoc_val = s7_cdr(assoc); */
/*     if (s7_is_proper_list(s7, assoc)) { */
/*         // first check for proper (undotted) lists */
/*         // (proper-list? '(:a . b)) => #f */
/*         // (proper-list? '(:a 1) => #t */
/*         // (proper-list? '(:a (1)) => #t */
/*         // (proper-list? '(:a 1 2 3)) => #t */
/*         // (proper-list? '(:a . (1 2 3))) => #t */
/*         // NB: (:a . (1 2 3)) evaluates to (:a 1 2 3)) */
/*         // (proper-list? '(:a (1 2 3))) => #t */
/*         // (proper-list? . ,(:lambda ...)) => #f */
/*         // (proper-list? ,(:lambda ...)) => #t */
/*         TRACE_S7_DUMP("assoc is " RED " proper list" CRESET, assoc); */
/*         TRACE_S7_DUMP("assoc-val", assoc_val); */

/*         // special handling: (:a 1 2 3) v. (:a (1 2 3)) */
/*         // cdr always a list */
/*         // (cdr (:a 1 2 3)) > (1 2 3) */
/*         // (cdr (:a (1 2 3))) > ((1 2 3)) */
/*         // in latter case, (cddr x) = '() */
/*         // but: (:a (1 2) (3 4)) - list of lists */
/*         // then val is ((1 2) (3 4)) */
/* #ifdef DEVBUILD */
/*         log_debug("assoc-val len: %d", s7_list_length(s7, assoc_val)); */
/* #endif */
/*         if (s7_list_length(s7, assoc_val) == 1) { */
/*             // case: (1)  from (:a 1) */
/*             s7_pointer car = s7_car(assoc_val); */
/*             if (s7_is_list(s7, car)) { */
/*                 // case: ((1)) */
/*                 // case: ((1 2 3)) */
/*                 // case: ((lambda ...)) */
/*                 TRACE_S7_DUMP("car is list", car); */
/*                 if (car == s7_nil(s7)) { */
/*                     selection = car; */
/*                 } */
/*                 else if (libs7_is_alist(s7, car)) { */
/*                     TRACE_S7_DUMP("car is alist", car); */
/*                     selection = car; */
/*                 } else { */
/*                     selection = assoc_val; */
/*                 } */
/*             } else { */
/*                 TRACE_S7_DUMP("car is NOT list", car); */
/*                 if (s7_is_procedure(car)) { */
/*                     TRACE_LOG_DEBUG("car is lambda", ""); */
/*                     if (e->stack[e->depth].index +1 < e->stack[e->depth].count) { */
/*                         e->lambda = true; */
/*                     } else { */
/*                         e->lambda = false; */
/*                         e->stack[e->depth].lambda = false; */
/*                     } */
/*                     e->stack[e->depth].lambda = true; */
/*                     selection = car; */
/*                 } */
/*                 else if (s7_is_vector(car)) { */
/* #ifdef DEVBUILD */
/*                     log_debug("car is vector"); */
/* #endif */
/*                     selection = car; */
/*                 } else { */
/* #ifdef DEVBUILD */
/*                     log_debug("car is nonlambda, nonvector"); */
/* #endif */
/*                     selection = car; */
/*                 } */
/*             } */
/*         } else { */
/* #ifdef DEVBUILD */
/*             log_debug("assoc_val len > 1"); */
/* #endif */
/*             selection = assoc_val; */
/*         } */
/*     } */
/*     else { */
/*         // improper list, cdr is never a list */
/*         /\* else if (s7_is_list(s7, assoc)) { *\/ */
/*         // DOTTED list, may be values or a proc */
/*         TRACE_S7_DUMP("assoc is " RED "improper list" CRESET, assoc); */
/*         TRACE_S7_DUMP("assoc-val", assoc_val); */
/*         // assoc_val never a list */
/*         if (s7_is_procedure(assoc_val)) { */
/*             TRACE_LOG_DEBUG("is lambda", ""); */
/*             e->stack[e->depth].lambda = true; */
/*             selection = assoc_val; */
/*             /\* if (s7_is_symbol(assoc_val)) { *\/ */
/*             /\*     s7_pointer x = s7_symbol_value(s7, assoc_val); *\/ */
/*             /\*     TRACE_S7_DUMP("x", x); *\/ */
/*             /\*     log_debug("proc? %d", s7_is_procedure(x)); *\/ */
/*             /\*     selection = x; *\/ */
/*             /\* } *\/ */
/*             /\* selection = s7_apply_function(s7, *\/ */
/*             /\*                       assoc_val, *\/ */
/*             /\*                       s7_nil(s7)); *\/ */
/*         } else { */
/* #ifdef DEVBUILD */
/*             log_debug("not lambda"); */
/* #endif */
/*             selection = assoc_val; */
/*         } */
/*     } */
/*     return selection; */
/* } */

/* called by wrap, closure is app struct tstack_s* */
static int sel(void *closure, const char *key)
{
    // key NULL means: select current obj (topmost in stack?)
    TRACE_ENTRY(sel);
    struct tstack_s *e = closure;
    e->stack[e->depth].predicate = e->predicate;

#ifdef DEVBUILD
    log_trace("key: %s", key);
    log_trace("predicate: %d", e->predicate);
    /* DUMP_CLOSURE(e, e->depth); */
#endif
    s7_pointer selection, assoc;
    int i=0, r=0; // r: found? boolean


    if (key == NULL) {
#ifdef DEVBUILD
        log_debug("e->predicate: %d", e->predicate);
        /* log_debug("NULL; final pred: %d", e->final_predicate); */
        /* log_debug("NULL; nonfinal pred: %d", e->nonfinal_predicate); */
#endif
        if (e->predicate) {
            switch(e->predicate) {
            case FIRST_P:
#ifdef DEVBUILD
                log_debug("case: FIRST_P");
#endif
                e->stack[e->depth].predicate = e->predicate;
                if (e->stack[e->depth].index == 0) {
#ifdef DEVBUILD
                    log_debug("predicate is truthy!");
#endif
                    selection = s7_t(s7);
                } else {
#ifdef DEVBUILD
                    log_debug("predicate is false!");
#endif
                    selection = s7_f(s7);
                }
                r = 1;
                break;
            case LAST_P:
#ifdef DEVBUILD
                log_debug("case: LAST_P");
#endif
                e->stack[e->depth].predicate = e->predicate;
                if (e->stack[e->depth].index + 1 < e->stack[e->depth].count) {
#ifdef DEVBUILD
                    log_debug("predicate is false!");
#endif
                    selection = s7_f(s7);
                } else {
#ifdef DEVBUILD
                    log_debug("predicate is truthy!");
#endif
                    selection = s7_t(s7);
                }
                r = 1;
                break;
            case BUTLAST_P:
#ifdef DEVBUILD
                log_debug("case: BUTLAST_P");
#endif
                e->stack[e->depth].predicate = e->predicate;
                // true for all but last
                if (e->stack[e->depth].index + 1 < e->stack[e->depth].count) {
                    /* s7_pointer tmp = s7_vector_ref(s7, e->stack[e->depth].ctx, e->stack[e->depth].index); */
                    /* (void)tmp; */
#ifdef DEVBUILD
                    log_debug("predicate is truthy!");
#endif
                    selection = s7_t(s7);
                } else {
#ifdef DEVBUILD
                    log_debug("pred {{?}} is false!");
#endif
                    selection = s7_f(s7);
                }
                r = 1;
                break;
            default:
                selection = s7_f(s7);
                r = 1;
            }
        }
        else {
            // null key, no meta means '.'
            TRACE_S7_DUMP("NULL key, selecting obj", e->stack[e->depth].obj);
            TRACE_LOG_DEBUG("iterating? %d", e->stack[e->depth].iterating);
            if (s7_is_pair(e->stack[e->depth].obj)) {
                if (e->stack[e->depth].iterating) {
                    // project val of kv pair
                    selection = s7_cdr(e->stack[e->depth].obj);
                } else {
                    selection = e->stack[e->depth].obj;
                }
                /* selection = s7_cadr(e->stack[e->depth].obj); */
            } else {
                selection = e->stack[e->depth].obj;
            }
            r = 1;
        }
    }
    else if (strcmp(key, "?") == 0) {
        if (e->stack[e->depth].index +1 < e->stack[e->depth].count) {
#ifdef DEVBUILD
            log_debug("ADD SMART COMMA");
#endif
            selection = s7_t(s7);
            r = 1; // sel S_ok?
        } else {
#ifdef DEVBUILD
            log_debug("OMIT SMART COMMA");
#endif
            selection = s7_f(s7);
            r = 0;
        }
    } else if (strcmp(key, "$") == 0) {
        if (e->stack[e->depth].index +1 < e->stack[e->depth].count) {
#ifdef DEVBUILD
            log_debug("ADD SMART COMMA");
#endif
            selection = s7_t(s7);
            r = 1; // sel S_ok?
        } else {
#ifdef DEVBUILD
            log_debug("OMIT SMART COMMA");
#endif
            selection = s7_f(s7);
            r = 0;
        }
    } else {
        i = e->depth;
        // find obj in stack matching key arg
        // FIXME: should we search obj or ctx?
        // e->stack[i].obj is an s7_pointer for a json object
        // (so it should be an assoc-list?)
        // to match, key must be a string fld in the obj
        // i.e. must be a key
        // cJSON_GetObjectItemCaseSensitive(obj, key) (cJSON)
        // json_object_object_get_ex (json-c)
        // json_object_get (jansson)
        // scm: (assoc key obj) ???

        //FIXME: use make_symbol even for :foo kw keys?
        s7_pointer key_kw;
        if (key[0] == ':') {
            key_kw = s7_make_symbol(s7, key+1);
        } else {
            // {{foo}} =>  :foo key
            key_kw = s7_make_keyword(s7, key);
        }
        TRACE_S7_DUMP("key_kw", key_kw);
        //FIXME: first search current stackframe (including context), then rest of stack
        // frames below top represent selected vals
        // search should query just that, or the ctx?

        //FIXME: accept both kws and syms? I.e. given {{foo}} search for :foo, then 'foo then "foo"?
        // clostache assumes data keys are kws, which seems to work well

#ifdef DEVBUILD
        log_debug("searching stack, height: %d, for key %s", i, key);
#endif
        /*
          SPECIAL CASE: root table objiter: {{#^.*}}...{{/^.*}}
         */
        if ((strncmp(key, "^", 1) == 0) && strlen(key) == 1) {
            TRACE_S7_DUMP("^ selecting", e->stack[i].obj);
            selection = e->stack[i].obj;
        } else {
            while (i >= 0) {
#ifdef DEVBUILD
                log_debug(GRN "stackframe:" CRESET " %d", i);
                /* DUMP_CLOSURE(e, i); */
#endif
                TRACE_S7_DUMP("e->stack[i].ctx", e->stack[i].ctx);
                TRACE_S7_DUMP("e->stack[i].obj", e->stack[i].obj);

                if (s7_is_hash_table(e->stack[i].obj)) {
                    TRACE_LOG_DEBUG("HASH-TABLE", "");
                    selection = s7_hash_table_ref(s7, e->stack[i].obj, key_kw);
                    if (selection != s7_f(s7)) {
#ifdef DEVBUILD
                        log_debug("HIT HT ENTRY at %d", i);
                        TRACE_S7_DUMP("selected", selection);
                        log_debug("predicate: %d", e->predicate);
#endif
                        /* selection = s7_cdr(selection); */
                        /* if (e->predicate) { */
                        /*     selection = _handle_stack_predicate_ht(e, selection); */
                        /*     break; */
                        /* } else { */
                        /*     selection = _handle_stack_match_ht(e, selection); */
                        /* } */

                        /* log_debug("HIT ONE at %d", i); */
                        break;
                    } else {
                        /* log_debug("MISS at %d", i); */
                    }

                }
                else if (libs7_is_alist(s7, e->stack[i].obj)) { // .ctx?
                    TRACE_LOG_DEBUG("ALIST", "");
                    if (key_kw == s7_make_keyword(s7, "^")) {
                        log_debug("XXXXXXXXXXXXXXXX");
                    } else {
                        assoc = s7_assoc(s7, key_kw, e->stack[i].obj); // .ctx?
#ifdef DEVBUILD
                        TRACE_S7_DUMP("assoc result", assoc);
#endif
                        if (assoc != s7_f(s7)) {
#ifdef DEVBUILD
                            log_debug("HIT ASSOC at %d", i);
                            log_debug("predicate: %d", e->predicate);
#endif
                            TRACE_S7_DUMP("assoc", assoc);

                            if (e->predicate) {
                                selection = _handle_stack_predicate(e, assoc);
                                break;
                            } else {
                                selection = _handle_stack_match(e, assoc);
                            }
                            break;
                        } else {
#ifdef DEVBUILD
                            log_debug("MISS at %d", i);
#endif
                            selection = s7_f(s7);
                        }
                    }
                }
                // not alist
                else if (s7_is_list(s7, e->stack[i].obj)) {
                    TRACE_LOG_DEBUG("LIST (not alist)", "");
                    TRACE_S7_DUMP("e->stack[i].obj", e->stack[i].obj);

                    if (key_kw == s7_car(e->stack[i].obj)) {
#ifdef DEVBUILD
                        log_debug("MATCH at stackframe %d", i);
#endif
                        selection = s7_cdr(e->stack[i].obj);
                        break;
                    } else {
                        TRACE_S7_DUMP("mismatch on obj", e->stack[i].obj);
                        assoc = s7_assoc(s7, key_kw, e->stack[i].ctx);
                        if (assoc != s7_f(s7)) {
                            TRACE_S7_DUMP("MATCH on ctx", assoc);
                            if (e->predicate) {
                                selection = _handle_stack_predicate(e, assoc);
                                break;
                            } else {
                                selection = _handle_stack_match(e, assoc);
                                TRACE_S7_DUMP("stack match", selection);
                                break;
                            }
                        } else {
                            TRACE_S7_DUMP("MISMATCH on ctx", e->stack[i].obj);
                        }
                    }
                }
                i--;
            }
        }
        /* log_debug("BROKE at %d", i); */
        fflush(NULL);
        if (i >= 0)
            r = 1;
        else {
            /* should this be s7_f(s7)? */
            selection = s7_unspecified(s7); //&e->null;
            r = 0;
        }
    }
#ifdef DEVBUILD
    TRACE_S7_DUMP("matched selection", selection);
    /* matched val should be alist or vector, not list? */
    TRACE_S7_DUMP("type", s7_type_of(s7, selection));
    log_debug("alist? %d", libs7_is_alist(s7, selection));
    /* log_debug("string? %d", s7_is_string(selection)); */
    /* log_debug("string? %d", s7_is_boolean(selection)); */
#endif

    // now assign selected datum to e->selection

    /* if data is dotted, e.g. ((:names . #(((:name . Joe))))),
       then match is #(((:name . Joe)))
       but if data is undotted, e.g ((:names #(((:name Joe))))),
       then match is (#(((:name Joe))))
       in the latter case we take the cdr.
       but if data is a map, e.g. ((:names ((:name Joe)))),
       then match is ((:name Joe))
       FIXME: so our policy is to treat lists as dotted pairs,
       e.g. ((:foo [1 2 3])) == ((:foo . [1 2 3]))
       What if we want the list?
     */
    if (s7_is_vector(selection)) {
        TRACE_S7_DUMP("selection is vector", selection);
        e->selection = selection;
    }
    if (s7_is_list(s7, selection)) {
        TRACE_S7_DUMP("selection is list", selection);
        if (s7_is_null(s7, selection)) {
            TRACE_S7_DUMP("null", selection);
            e->selection = selection;
            r = 0;              /* did not select elt */
        }
        else if (libs7_is_alist(s7, selection)) {
            TRACE_S7_DUMP("selection is alist", selection);
            // e.g. (:nbrs (1 2 3)), ((:name Joe))
            e->selection = selection;
            /* e->selection = s7_car(selection); */
        }
        else {
            TRACE_S7_DUMP("selection is not alist", selection);
            // convert list to vector?
            e->selection = selection;
            // e.g. (:nbrs (1 2 3)), selection == ((1 2 3))
            /* s7_pointer car = s7_car(selection); */
            /* if (s7_is_list(s7, car)) { */
            /*     e->selection = s7_car(selection); */
            /* } */
            /* else if (s7_is_vector(car)) { */
            /*     e->selection = s7_car(selection); */
            /* } */
            /* else if (s7_is_boolean(car)) { */
            /*     e->selection = car; */
            /* } else { */
            /*     e->selection = selection; */
            /* } */
        }
    }
    else if (s7_is_boolean(selection)) {
        TRACE_S7_DUMP("bool selection", selection);
        e->selection = selection;
        /* if (selection == s7_t(s7)) */
        /*     e->selection = s7_t(s7); */
        /* else */
        /*     e->selection = s7_f(s7); */
        /* bool b = s7_boolean(s7, selection); */
        /* log_debug("BOOL: %d", b); */
        /* return(b); */
    }
    else if (s7_is_procedure(selection)) {
        e->stack[e->depth].lambda = true;
        TRACE_S7_DUMP("procedure selected", selection);
        e->selection = selection;
        r = 1;
    }
    else if (s7_is_unspecified(s7, selection)) {
        TRACE_S7_DUMP("unspecified selection", selection);
        e->selection = selection;
        r = 0;
    }
    else {
        TRACE_S7_DUMP("selection type", s7_type_of(s7, selection));
        TRACE_S7_DUMP("selection val", selection);
        e->selection = selection;
    }
#ifdef DEVBUILD
    TRACE_S7_DUMP("set SELECTION", e->selection);
    /* log_debug("e->predicate: %d", e->predicate); */
    /* log_debug("e->stack[%d].predicate: %d", e->depth, e->stack[e->depth].predicate); */
    /* log_debug("e->stack[%d].lambda: %d", e->depth, e->stack[e->depth].lambda); */
    dump_stack(e);
#endif
    return r;
}

/* for segments in dotted key, e.g. name of {{person.name}} */
static int subsel(void *closure, const char *name)
{
    TRACE_ENTRY(subsel);
#ifdef DEVBUILD
    log_debug("\tkey: '%s'", name);

#endif
    struct tstack_s *e = closure;
#ifdef DEVBUILD
    /* DUMP_CLOSURE(e, e->depth); */
#endif
    s7_pointer o;
    int r = 0;

    /* s7_GetObjectItemCaseSensitive(e->selection, name); */
    /* json_object_object_get_ex */
    /* json_object_get */
    s7_pointer key = s7_make_keyword(s7, name);
    if (s7_is_hash_table(e->selection)) {
        TRACE_S7_DUMP("subsel: ht", e->selection);
        o = s7_hash_table_ref(s7,e->selection, key);
        if (o == s7_f(s7)) {
            r = 0;
        } else {
            e->selection = o;
            r = 1;
        }
    } else if (libs7_is_alist(s7, e->selection)) {
        TRACE_S7_DUMP("subsel alist", e->selection);
        o = s7_assoc(s7, key, e->selection);
        if (o == s7_f(s7)) {
            r = 0;
        } else {
            TRACE_S7_DUMP("subselected", o);
            s7_pointer val = s7_cdr(o);
            TRACE_S7_DUMP("val", val);
            // (:msg . "hello") val =  "hello"
            // (:msg "hello") val =  ("hello")
            // (:xs 1 2 3) val = (1 2 3)
            // (:b . (:msg "hello")) val = ((:msg "hello"))
            // (:b (:msg "hello")) val = ((:msg "hello"))
            if (s7_is_list(s7, val)) {
                if (libs7_is_alist(s7, val)) {
                    // val = ((:msg "hello"))
                    e->selection = val;
                }
                else if (s7_list_length(s7, val) == 1) {
                    // treat singleton list as val not list
                    e->selection = s7_car(val);
                } else {
                    // (1 2 3)
                    e->selection = val;
                }
            } else {
                // (:msg . "hello")
                e->selection = val;
            }
            r = 1;
        }
    } else if (s7_is_list(s7, e->selection)) {
        TRACE_S7_DUMP("subsel list", e->selection);
        o = s7_assoc(s7, key, e->selection);
        if (o == s7_f(s7)) {
            e->selection = o;
            r = 0;
        } else {
            TRACE_S7_DUMP("subselelected", o);
            e->selection = s7_cdr(o);
            r = 1;
        }
    } else {
        o = s7_undefined(s7);
    }
    TRACE_S7_DUMP("subsel result", e->selection);
    /* r = o != s7_f(s7); */
    /* if (r) */
    return r;
}

/* enter
   task: create new stack frame IF most recent selection is collection
   returns: 1 if new stack frame created (e->selection is collection)
   collections: map, vector, list
 */
static int enter(void *closure, int objiter)
{
    TRACE_ENTRY(enter);
    TRACE_LOG_DEBUG("objiter? %d", objiter);
#ifdef DEVBUILD
    log_debug("predicate: 0x%04X",
              ((struct closure_hdr*)closure)->predicate);
#endif
    struct tstack_s *e = closure;
    s7_pointer selected;

#ifdef DEVBUILD
    /* DUMP_CLOSURE(e, e->depth); */
#endif

    /* if (!e->lambda) { */
#ifdef DEVBUILD
    log_debug("incrementing stackframe idx from %d", e->depth);
#endif
    e->depth++;
    if (e->depth >= MUSTACH_MAX_DEPTH)
        return MUSTACH_ERROR_TOO_DEEP;
    /* } */
    selected = e->selection;
    e->stack[e->depth].iterating = 0;
    if (e->predicate) {
        e->stack[e->depth].predicate = e->predicate;
#ifdef DEVBUILD
        log_debug("selection type: PREDICATE");
        TRACE_S7_DUMP("selected", selected);
        log_debug("w->predicate: %d", e->predicate);
#endif
        if ((selected == s7_unspecified(s7)) || selected == s7_f(s7)) {
            goto not_entering;
        } else { // truthy
            e->stack[e->depth].count = 1;
            e->stack[e->depth].ctx = s7_nil(s7);
            e->stack[e->depth].obj = selected;
            e->stack[e->depth].index = 0;
        }
    }
    else if (objiter) { // objiter is boolean?
        TRACE_LOG_DEBUG("OBJITER", "");
        TRACE_S7_DUMP("selected", selected);
        if (s7_is_hash_table(selected)) {
            TRACE_LOG_DEBUG("HT", "");
            s7_pointer env = s7_inlet(s7,
                                      s7_list(s7, 1,
                                              s7_cons(s7,
                                                      s7_make_symbol(s7, "ht"),
                                                      selected)));
            char *sexp = "(hash-table-entries ht)";
            s7_pointer ht_ct_
                = s7_eval_c_string_with_environment(s7, sexp, env);
            int ht_ct = s7_number_to_integer(s7, ht_ct_);
            TRACE_LOG_DEBUG("ht ct: %d", ht_ct);
            if (ht_ct < 1)
                goto not_entering;
            s7_pointer iter = s7_make_iterator(s7, selected);
            if (!s7_is_iterator(iter)) {
                log_error("s7_make_iterator failed for hash table");
                goto not_entering;
            }
            s7_pointer mapentry = s7_iterate(s7, iter);
            if (s7_iterator_is_at_end(s7, iter)) {
                // ht is empty?
                goto not_entering;
            }
            e->stack[e->depth].iter = iter;
            //FIXME: gc_protect?
#ifdef DEVBUILD
            log_debug("iterating, stackframe %d: hash-table", e->depth);
#endif
            e->stack[e->depth].ctx = selected;
            e->stack[e->depth].obj = mapentry;
            e->stack[e->depth].iterating = 1;
        }
        else if (libs7_is_alist(s7, selected)) {
            /* log_debug("ALIST"); */
            // populate stackframe with first mapentry
            int ct = s7_list_length(s7, selected);
            /* log_debug("alist ct: %d", ct); */
            if (ct < 1)
                goto not_entering;
            /* e->stack[e->depth].iter = s7_make_iterator(s7, selected); */
            e->stack[e->depth].count = ct;
            e->stack[e->depth].index = 0;
            e->stack[e->depth].ctx = selected;
            e->stack[e->depth].obj = s7_car(selected);
            e->stack[e->depth].iterating = 1;
        } else {
            log_debug("NOT MAP");
            goto not_entering;
        }
    }
    else if (s7_is_procedure(selected)) { /* LAMBDA */
#ifdef DEVBUILD
        log_debug("selection type: LAMBDA");
#endif
        e->lambda = true;
        e->stack[e->depth].lambda = true;
        e->stack[e->depth].workbuf_idx = ++workbufs_tos;
        strlcpy(workbuf_stack[workbufs_tos].buf, "Test", 5);
        e->stack[e->depth].count = 1;
        // context is obj of prev. stackframe?
        e->stack[e->depth].ctx = e->stack[e->depth-1].obj;
        e->stack[e->depth].obj = selected;
        e->stack[e->depth].index = 0;
    }
    // json impls only check for array, not obj/table
    else if (s7_is_vector(selected)) {
#ifdef DEVBUILD
        log_debug("selection type: VECTOR");
        /* DUMP("vec", selected); */
#endif
        e->stack[e->depth].count = s7_vector_length(selected);
        if (e->stack[e->depth].count == 0)
            goto not_entering;
        e->stack[e->depth].ctx = selected;
        e->stack[e->depth].obj = s7_vector_ref(s7, selected, 0);
        e->stack[e->depth].index = 0;

    }
    else if (s7_is_list(s7, selected)) { /* LIST */
#ifdef DEVBUILD
        log_debug("selection type: LIST");
        /* DUMP("list", selected); */
#endif
        if (s7_is_null(s7, selected)) {
#ifdef DEVBUILD
            log_debug("selection type: LIST (NULL)");
#endif
        }
        else if (libs7_is_kw_alist(s7, selected)) { /* ALIST */
#ifdef DEVBUILD
            TRACE_S7_DUMP("selected", selected);
            log_debug("selection type: ALIST");
            log_debug("setting stackframe for selection type: ALIST (NON-NULL)");
#endif
            /* e->stack[e->depth].count = s7_list_length(s7, selected); */
            /* if (e->stack[e->depth].count == 0) */
            /*     goto not_entering; */
            // only set count to list length for non-alists
            e->stack[e->depth].count = 1;
            e->stack[e->depth].ctx = s7_nil(s7);
            e->stack[e->depth].obj = selected;
            /* e->stack[e->depth].ctx = selected; */
            /* e->stack[e->depth].obj = s7_car(selected); */
            e->stack[e->depth].index = 0;
        }
        else if (s7_is_proper_list(s7, selected)) {
#ifdef DEVBUILD
            log_debug("setting stackframe for selection type: PROPER LIST (NON-NULL)");
#endif
            e->stack[e->depth].count = s7_list_length(s7, selected);
            if (e->stack[e->depth].count == 0)
                goto not_entering;
            e->stack[e->depth].ctx = selected;
            e->stack[e->depth].obj = s7_car(selected);
            e->stack[e->depth].index = 0;
        } else {
            TRACE_S7_DUMP("selected", selected);
#ifdef DEVBUILD
            log_debug("setting stackframe for selection type: IMPROPER LIST (NON-NULL)");
#endif
            // improper list length: -1
            goto not_entering;
            /* if (e->stack[e->depth].count == 0) */
            /* e->stack[e->depth].ctx = selected; */
            /* e->stack[e->depth].obj = s7_car(selected); */
            /* e->stack[e->depth].index = 0; */
        }
    }
    else if (selected == s7_unspecified(s7)) {
#ifdef DEVBUILD
        log_debug("selection: UNSPECIFIED");
#endif
        e->stack[e->depth].count = 1;
        e->stack[e->depth].ctx = selected;
        e->stack[e->depth].obj = selected;
        e->stack[e->depth].index = 0;
        goto not_entering;
    }
    else if (selected == s7_t(s7)) {
#ifdef DEVBUILD
        log_debug("selection: TRUE");
#endif
        if (e->predicate) {
            e->stack[e->depth].predicate = e->predicate;
        } else {
            e->stack[e->depth].count = 1;
            e->stack[e->depth].ctx = selected;
            e->stack[e->depth].obj = selected;
            e->stack[e->depth].index = 0;
            /* goto not_entering; */
        }
    }
    else if (selected != s7_f(s7) && !s7_is_null(s7, selected)) {
        // could be map, boolean #t, number, string, etc.
#ifdef DEVBUILD
        log_debug("selection TRUTHY");
        /* DUMP("ATOM", selected); */
        log_debug("e->lambda? %d", e->lambda);
        log_debug("e->stack[%d].lambda? %d", e->depth, e->stack[e->depth].lambda);
#endif
        e->stack[e->depth].count = 1;
        e->stack[e->depth].ctx = s7_nil(s7); // prevent ctx search?
        e->stack[e->depth].obj = selected;
        e->stack[e->depth].index = 0;
    }
    else {
#ifdef DEVBUILD
        log_debug("selection type: OTHER");
#endif
        goto not_entering;
    }
#ifdef DEVBUILD
    /* DUMP_CLOSURE(e, e->depth); */
    log_debug("returning: ENTERED");
#endif
    /* log_debug("RETURNING"); */
    /* DUMP("obj t is ht?", s7_is_hash_table(e->stack[e->depth].obj)); */
    return 1;

 not_entering:
    /* if (!e->lambda) */
    e->stack[e->depth].count = 0;
    e->stack[e->depth].index = 0;
    e->stack[e->depth].iterating = 0;
    e->depth--;
#ifdef DEVBUILD
    log_debug("NOT ENTERED");
    log_debug("decrement stackframe idx to %d", e->depth);
    /* DUMP_CLOSURE(e, e->depth); */
#endif
    /* log_debug("RETURNING w/o entry"); */
    return 0;
}

/* next: increment index (& set obj) of current stackframe */
static int next(struct tstack_s *e) // closure)
{
    TRACE_ENTRY(next)
	/* struct tstack_s *e = closure; */
    /* s7_pointer o; */
#ifdef DEVBUILD
    /* DUMP_CLOSURE(e, e->depth); */
    log_debug("e->depth: %d", e->depth);
    log_debug("e->stack[%d].index: %d", e->depth, e->stack[e->depth].index);
    log_debug("e->stack[%d].count: %d", e->depth, e->stack[e->depth].count);
    dump_stack(e);
#endif

    if (e->depth <= 0) {
        // FIXME: why is this an error?
        log_error("next: depth <= 0: %d", e->depth);
        fflush(NULL);
        /* exit(EXIT_FAILURE); */
        return MUSTACH_ERROR_CLOSING;
    }
    TRACE_LOG_DEBUG("stackframe: %d", e->depth);
    TRACE_LOG_DEBUG("stackframe ct: %d", e->stack[e->depth].count);
    /* o = e->stack[e->depth].next; */
    /* if (o == s7_nil(s7)) */
    /* 	return 0; */
    if (e->stack[e->depth].iterating) {
        if (e->stack[e->depth].iter) {
            s7_pointer item = s7_iterate(s7, e->stack[e->depth].iter);
            if (s7_iterator_is_at_end(s7, e->stack[e->depth].iter))
                return 0;
            e->stack[e->depth].obj = item;
            return 1;
        } else {
            e->stack[e->depth].index++;
            if (e->stack[e->depth].index >= e->stack[e->depth].count)
                return 0;
            // set key, obj to next mapitem
            s7_pointer x = s7_list_ref(s7, e->stack[e->depth].ctx,
                                       e->stack[e->depth].index);
            TRACE_S7_DUMP("next:", x);
            e->stack[e->depth].obj = x;
            return 1; // has_next
        }
    }

    e->stack[e->depth].index++;
    if (e->stack[e->depth].index >= e->stack[e->depth].count) {
#ifdef DEVBUILD
        log_debug("incremented index to %d (end of list)", e->stack[e->depth].index);
#endif
        return 0; // !has_next
    }
    if (s7_is_vector(e->stack[e->depth].ctx)) {
        e->stack[e->depth].obj = s7_vector_ref(s7, e->stack[e->depth].ctx, e->stack[e->depth].index);
    }
    else if (s7_is_list(s7, e->stack[e->depth].ctx)) {
        e->stack[e->depth].obj = s7_list_ref(s7, e->stack[e->depth].ctx, e->stack[e->depth].index);
    }

#ifdef DEVBUILD
    log_debug("incremented stackframe index to %d", e->stack[e->depth].index);
    log_debug("stackframe ct: %d", e->stack[e->depth].count);
#endif
    return 1; // has_next
}

static int leave(void *closure, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(leave)
        struct tstack_s *e = closure;
#ifdef DEVBUILD
    log_debug("current stackframe");
    /* DUMP_CLOSURE(e, e->depth); */
    /* log_debug("decrementing stackframe index from %d", e->depth); */
#endif

    if (e->depth <= 0) {
        log_debug("cXXXXXXXXXXXXXXXX");
        return MUSTACH_ERROR_CLOSING;
    }
    if (e->stack[e->depth].lambda) {
        log_debug("LAMBDA");
#ifdef DEVBUILD
        /* log_debug("workbuf tos: %d", workbufs_tos); */
        /* log_debug("lambda workbuf idx: %d", (e->stack[e->depth].workbuf_idx)); */
        /* log_debug("lambda workbuf: %s", (workbuf_stack[e->stack[e->depth].workbuf_idx].buf)); */
#endif
        s7_pointer lambda = e->stack[e->depth].obj;
        s7_pointer arity = s7_car(s7_arity(s7, lambda));
        if (s7_integer(arity) != 1) {
            log_error("Bad arity; expected 1, actual: %d", s7_integer(arity));
            exit(EXIT_FAILURE);
        }
        s7_pointer arg = s7_make_string(s7,
                                        workbuf_stack[e->stack[e->depth].workbuf_idx].buf);
        s7_pointer result = s7_apply_function(s7, lambda, s7_list(s7, 1,
                                                                  arg));
        TRACE_S7_DUMP("lambda result", result);
        // now put result in sbuf
        const char *s = s7_string(result);
        sbuf->value = s;
        sbuf->freecb = free;    /* FIXME: why? */
    }
#ifdef DEVBUILD
    log_debug("decrementing e->depth from %d", e->depth);
#endif
    e->stack[e->depth].count = 0;
    e->stack[e->depth].index = 0;
    e->stack[e->depth].iterating = 0;
    e->depth--;
    return 0;
}

static bool _list_is_empty_rec(s7_scheme *s7, s7_pointer lst);
static bool _vector_is_empty_rec(s7_scheme *s7, s7_pointer vec);

static bool _hash_table_is_empty_rec(s7_scheme *s7, s7_pointer ht)
{
    TRACE_ENTRY("_hash_table_is_empty_rec");
    s7_pointer ct = s7_call(s7,
                            s7_name_to_value(s7, "hash-table-entries"),
                            s7_list(s7, 1,  ht));
    if (s7_integer(ct) == 0) {
        return true;
    }

    s7_pointer mapentry;
    s7_pointer iter = s7_make_iterator(s7, ht);
    if (!s7_is_iterator(iter)) {
        log_error("s7_make_iterator failed for hash table");
    }
    mapentry = s7_iterate(s7, iter);
    while (!s7_iterator_is_at_end(s7, iter)) {
        if (s7_is_list(s7, mapentry)) {
            if (_list_is_empty_rec(s7, mapentry)) {
                continue;
            } else {
                return false;
            }
        }
        else if (s7_is_vector(mapentry)) {
            if (_vector_is_empty_rec(s7, mapentry)) {
                continue;
            } else {
                return false;
            }
        }
        else if (s7_is_hash_table(mapentry)) {
            if (_hash_table_is_empty_rec(s7, mapentry)) {
                continue;
            } else {
                return false;
            }
        }
        mapentry = s7_iterate(s7, iter);
    }
    return true;
}

static bool _vector_is_empty_rec(s7_scheme *s7, s7_pointer vec)
{
    if (s7_vector_length(vec) == 0) {
        return true;
    } else {
        s7_int ct = s7_vector_length(vec);
        s7_pointer *elts = s7_vector_elements(vec);
        for (int i = 0; i < ct; i++) {
            TRACE_S7_DUMP("elts[i]", elts[i]);
            if (s7_is_list(s7, elts[i])) {
                if (s7_list_length(s7, elts[i]) == 0) {
                    continue;
                } else {
                    // does non-empty list contain only empties?
                    if (_list_is_empty_rec(s7, elts[i])) {
                        continue;
                    } else {
                        return false;
                    }
                }
            }
            else if (s7_is_vector(elts[i])) {
                if (_vector_is_empty_rec(s7, elts[i])) {
                    continue;
                } else {
                    return false;
                }
            }
            else if (s7_is_hash_table(elts[i])) {
                if (_hash_table_is_empty_rec(s7, elts[i])) {
                    continue;
                } else {
                    return false;
                }
            } else {
                // elts[i] neither list nor vector
                return false;
            }
        } // end for loop
        return true;
    }
}

static bool _list_is_empty_rec(s7_scheme *s7, s7_pointer lst)
{
    if (lst == s7_nil(s7)) {
        return true;
    } else {
        s7_pointer car = s7_car(lst);
        if (s7_is_vector(car)) {
            if (s7_vector_length(car) == 0) {
                return _list_is_empty_rec(s7, s7_cdr(lst));
            } else {
                // does non-empty vector contain only empties?
                return (_vector_is_empty_rec(s7, car)
                        && _list_is_empty_rec(s7, s7_cdr(lst)));
            }
        } else if (s7_is_list(s7, car)) {
            return (_list_is_empty_rec(s7, car)
                    && _list_is_empty_rec(s7, s7_cdr(lst)));
        } else {
            // car is neither list nor vector
            return false;
        }
    }
}

/* format e->selection it sbuf.value, for printing? */
/* rc 0: no val? */
static int format(struct tstack_s *stack, const char *fmt,
                  struct mustach_sbuf *sbuf, int key)
// (void *closure, struct mustach_sbuf *sbuf, int key)
{
    TRACE_ENTRY(format)
    TRACE_LOG_DEBUG("key: %d", key);
    TRACE_LOG_DEBUG("fmt: %s", fmt);
    TRACE_LOG_DEBUG("SBUF %p", sbuf);
    /* struct tstack_s *e = closure; */
    const char *s;

    char work[512]; // for formatting
    int len;

#ifdef DEVBUILD
    log_debug("key: %d", key);
    /* log_debug("\tsbuf->releasecb: %x", sbuf->releasecb); */
    /* DUMP_CLOSURE(e, stack->depth); */
#endif
    /* TRACE_S7_DUMP("selection", stack->selection); */

    if (key > 0) { // key is boolean?
#ifdef DEVBUILD
        log_debug("objiter?: %d", key);
#endif
        if (stack->stack[stack->depth].iterating) {
            /* log_debug("iterating"); */
            //FIXME: is obj guaranteed to be a pair?
            TRACE_S7_DUMP("stack obj", stack->stack[stack->depth].obj);
            s7_pointer x = s7_car(stack->stack[stack->depth].obj);
            s7_pointer k;
            if (s7_is_keyword(x)) {
                k = s7_keyword_to_symbol(s7, x);
            } else {
                k = x;
            }
            s = s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                      s7_make_string(s7, "~A"),
                                      k));
            /* log_debug("FORMATTED: %s", s); */
        } else {
            /* log_debug("NO iterating"); */
            s = "";
        }
    }
    else if (s7_is_vector(stack->selection)) {
        TRACE_LOG_DEBUG("format: selection is vector", "");
        TRACE_S7_DUMP("vec", stack->selection);
        if (_vector_is_empty_rec(s7, stack->selection)) {
            s = "";
        } else {
            char *fmt = "~A"; // "~{~A~^ ~}";
            /* char *fmt = "[~{~S~^ ~}]"; */
            s = s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                      s7_make_string(s7, fmt),
                                      stack->selection));
        }
    }
    else if (s7_is_hash_table(stack->selection)) {
        TRACE_LOG_DEBUG("format: selection is hash-table", "");
        s7_pointer ct = s7_call(s7,
                                s7_name_to_value(s7, "hash-table-entries"),
                                s7_list(s7, 1, stack->selection));
        if (s7_integer(ct) == 0) {
            /* log_debug("EMPTY HT"); */
            s = "";
        } else {
            s = s7_object_to_c_string(s7, stack->selection);
            if (s == NULL) return MUSTACH_ERROR_SYSTEM;
            sbuf->freecb = free;    /* FIXME: why? */
        }
    }
    else if (libs7_is_alist(s7, stack->selection)) {
        TRACE_LOG_DEBUG("format: selection is alist", "");
        TRACE_S7_DUMP("selection", stack->selection);
        // do NOT use s7's format fn to remove meta-notation
        // e.g. '(1 2 3)' => '1 2 3'
        // ~^ stops at end of sequence
        // ~| stops after (*s7* 'print-length) elements

        if (_list_is_empty_rec(s7, stack->selection)) {
            s = "";
        /* } else { */
        /* if (stack->selection == s7_nil(s7)) { */
        /*     s = ""; */
        /* } else if (libs7_is_empty_alist(s7, stack->selection)) { */
        /*     s = ""; */
        } else {
            char *fmt = "~A"; // "~{~A~^ ~}";
            s = s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                      s7_make_string(s7, fmt),
                                      stack->selection));
        }
    }
    else if (s7_is_list(s7, stack->selection)) {
        TRACE_LOG_DEBUG("format: selection is list", "");
        TRACE_S7_DUMP("selection", stack->selection);
        if (_list_is_empty_rec(s7, stack->selection)) {
            s = "";
        } else {
        /* if (stack->selection == s7_nil(s7)) { */
        /*     s = ""; */
        /* } else { */
            char *fmt = "~A"; // "~{~A~^ ~}";
            /* char *fmt = "[~{~A~^ ~}]"; */
            s = s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                      s7_make_string(s7, fmt),
                                      stack->selection));
            /* s = s7_object_to_c_string(s7, stack->selection); */
            /* if (s == NULL) return MUSTACH_ERROR_SYSTEM; */
            /* sbuf->freecb = free;    /\* FIXME: why? *\/ */
        }
    }
    else if (s7_is_integer(stack->selection)) {
        TRACE_S7_DUMP("stack->selection is integer", stack->selection);
        //FIXME: formatting
        s = s7_object_to_c_string(s7, stack->selection);
        if (s == NULL) return MUSTACH_ERROR_SYSTEM;
        sbuf->freecb = free;
    }
    /* else if (s7_is_integer(stack->selection)) { */
    /*     TRACE_LOG_DEBUG("format: selection is integer: %s", */
    /*                     s7_string(stack->selection)); */
    /*     if (fmt) { */
    /*         //FIXME: apply fmt */
    /*     } else { */
    /*     } */
    /*     s = s7_number_to_string(s7, stack->selection, 10); */
    /* } */
    else if (s7_is_real(stack->selection)) {
        TRACE_LOG_DEBUG("format: selection is real: %s",
                        s7_string(stack->selection));
        double d = s7_real(stack->selection);
        if (fmt) {
            len = snprintf(NULL, 0, fmt, d);
            snprintf(work, len+1, fmt, d);
        } else {
            len = snprintf(NULL, 0, "%g", d);
            snprintf(work, len+1, "%g", d);
        }
        s = strndup(work, len+1);
    }
    else if (s7_is_string(stack->selection)) {
        TRACE_LOG_DEBUG("format: selection is string: %s",
                        s7_string(stack->selection));
        if (fmt) {
            //FIXME: apply fmt
        } else {
        }
        s = s7_string(stack->selection);
    }
    else if (s7_is_symbol(stack->selection)) {
#ifdef DEVBUILD
        log_debug("format: selection is symbol");
#endif
        s = s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                  s7_make_string(s7, "~A"),
                                  stack->selection));
    }
    else if (s7_is_null(s7, stack->selection)) {
#ifdef DEVBUILD
        log_debug("format: selection is null");
#endif
        s = "";
    }
    else if (s7_is_unspecified(s7, stack->selection)) {
#ifdef DEVBUILD
        log_debug("format: selection is unspecified - (values)?");
#endif
        /* s = ""; */
        s = NULL; // s7_object_to_c_string(s7, stack->selection);
        sbuf->value = s;
        /* log_debug("sbuf->value: %s", sbuf->value); */
        return 0;              /* ???? */
    }
    else if (s7_is_procedure(stack->selection)) {
#ifdef DEVBUILD
        log_debug("format: selection is procedure");
#endif
        sbuf->lambda = true;
        s7_pointer arity=s7_car(s7_arity(s7, stack->selection));
        s7_pointer result;
        if (s7_integer(arity) == 0) {
#ifdef DEVBUILD
            log_debug("ARITY: 0");
#endif
            result = s7_apply_function(s7, stack->selection, s7_nil(s7));
        }
        else if (s7_integer(arity) == 1) {
#ifdef DEVBUILD
            log_debug("ARITY: 1");
#endif
            result = s7_make_string(s7, "LAMBDA_RESULT");
            // Do not apply until we hit end tag
            /* if (s7_is_vector(stack->stack[stack->depth].ctx)) { // or list */
            /*     TRACE_S7_DUMP("CTX is vector", stack->stack[stack->depth].cont); */
            /*     if (stack->stack[stack->depth].index < stack->stack[stack->depth].count) { */
            /*         // stack->stack[stack->depth].obj is current selection? */
            /*         TRACE_S7_DUMP("Applying lambda to arg", stack->stack[stack->depth].obj); */
            /*         o = s7_apply_function(s7, */
            /*                               cadr, */
            /*                               s7_list(s7, 1, */
            /*                                       stack->stack[stack->depth].obj)); */
            /*         TRACE_S7_DUMP("applic result", o); */
            /*     } else { */
            /*         log_error("WTF?"); */
            /*         log_error("stack->stack[stack->depth].index: %d", stack->stack[stack->depth].index); */
            /*         log_error("stack->stack[stack->depth].count: %d", stack->stack[stack->depth].count); */
            /*         exit(EXIT_FAILURE); */
            /*     } */
            /* } */
        } else {
            log_error("Bad arity");
            // FIXME
            exit(EXIT_FAILURE);
        }
        /* char *s; */
        /* s = s7_object_to_c_string(s7, result); // must be freed */
        s = s7_string(result); // do NOT free
        sbuf->value = s;
        /* sbuf->freecb = free; */
    } else {
#ifdef DEVBUILD
        log_debug("format: else");
        TRACE_S7_DUMP("stack->selection", stack->selection);
#endif
        /* s = s7_PrintUnformatted(stack->selection); */
        /* s = json_dumps(stack->selection, JSON_ENCODE_ANY | JSON_COMPACT); */
        s = s7_object_to_c_string(s7, stack->selection);
        if (s == NULL) return MUSTACH_ERROR_SYSTEM;
        sbuf->freecb = free;    /* FIXME: why? */
    }
    /* log_debug("XXXXXXXXXXXXXXXX %p", sbuf); */
    /* log_debug("sbuf->value: %p", sbuf->value); */
    sbuf->value = s;
#ifdef DEVBUILD
    /* log_debug("\tsbuf->releasecb: %p", sbuf->releasecb); */
#endif
    return 1;
}

static void _dump_obj(char *msg, s7_pointer item)
{
    (void)msg;
    (void)item;
    TRACE_S7_DUMP(msg, item);
}

/* **************************************************************** */
static void dump_stack(struct tstack_s *stack)
{
    /* log_debug("stack ptr: %p", stack); */
    /* struct tstack_s *e = (struct tstack_s*)stack; */
    int d = stack->depth;
    log_debug("DUMP_STACK");
    log_debug("\tpredicate: %d", stack->predicate);
    log_debug("\tlambda: %d", stack->lambda);
    log_debug("\tdepth: %d", d);

    _dump_obj("\troot:", stack->root);
    _dump_obj("\tselection:", stack->selection);
    for (int i = 0; i <= stack->depth; i++) {
        log_debug("stackframe: %d", i);
        if (stack->stack[i].ctx)
            _dump_obj("\tctx:", stack->stack[i].ctx);
        if (stack->stack[i].obj)
            _dump_obj("\tobj", stack->stack[i].obj);
        log_debug("\tstack[%d].iterating: %d", i, stack->stack[i].iterating);
        log_debug("\tstack[%d].count: %d", i, stack->stack[i].count);
        log_debug("\tstack[%d].index: %d", i, stack->stack[i].index);
        log_debug("\tstack[%d].lambda: %d", i, stack->stack[i].lambda);
        log_debug("\tstack[%d].predicate: %d", i, stack->stack[i].predicate);
    }
    log_debug("end stack");
    fflush(NULL);
}

/* static void dump_stack(struct tstack_s *stack) */
/* { */
/*     int d = stack->depth; */
/*     log_debug("DUMP_STACK"); */
/*     log_debug("\tpredicate: %d", stack->predicate); */
/*     log_debug("\tlambda: %d", stack->lambda); */
/*     log_debug("\tdepth: %d", d); */
/*     TRACE_S7_DUMP("\troot", stack->root); */
/*     TRACE_S7_DUMP("\tselection", stack->selection); */
/*     if (stack->stack[d].ctx) */
/*         TRACE_S7_DUMP("\tstack->stack[%d].ctx", stack->stack[d].ctx); */
/*     else */
/*         log_debug("ctx: ?"); */
/*     if (stack->stack[d].obj) */
/*         TRACE_S7_DUMP("\tstack->stack[%d].obj", stack->stack[d].obj); */
/*     else */
/*         log_debug("obj: ?"); */
/*     log_debug("\tstack->stack[%d].count: %d", d, stack->stack[d].count); */
/*     log_debug("\tstack->stack[%d].index: %d", d, stack->stack[d].index); */
/*     log_debug("\tstack->stack[%d].lambda: %d", d, stack->stack[d].lambda); */
/*     log_debug("\tstack->stack[%d].predicate: %d", d, stack->stack[d].predicate); */
/*     log_debug("end stack"); */
/*     fflush(NULL); */
/* } */

// const struct mustach_wrap_itf mustach_wrap_itf_scm = {
const struct mustach_ds_methods_s scm_methods = {
    .start = (int (*)(void*))start,
    .stop = NULL,
    .compare = compare,
    .sel = sel,
    .subsel = subsel,
    .enter = enter,
    .next = (int (*)(void*))next,
    .leave = leave,
    .format = (int (*)(void*, const char*, struct mustach_sbuf*, int))format,
    .dump_stack = (void (*)(void*))dump_stack
};

/* ****************************************************************
 * PUBLIC RENDER API
 **************************************************************** */
/* returns rendered string */
const char *mustache_scm_render(const char *template,
                                size_t template_sz,
                                s7_pointer data,
                                int _flags)
                                /* errno_t* err) */
{
    //FIXME: put stack on the heap? mustache_new_stack()?
    struct tstack_s stack;
    memset(&stack, '\0', sizeof(struct tstack_s));
    stack.root = (s7_pointer)data;

    int flags = Mustach_With_All_SCM_Extensions;
    /* flags &= ~Mustach_With_JsonPointer; */
    (void)_flags;
    //FIXME: _flags arg is for opting out of default 'all'

    char *result = mustach_scm_render_to_string(template, template_sz,
                                                &scm_methods,
                                                &stack,
                                                flags);
                                                /* err); */
                                            /* &result, &result_sz); */
    /* if (*err != 0) { */
    if (result == NULL) {
        log_error("mustach_scm_render_to_string failure"); // : %d", *err);
        return NULL;
    } else {
        return result;
    }
}

int mustache_scm_frender(FILE *f,
                         const char *template,
                         size_t template_sz,
                         s7_pointer data,
                         int _flags)
{
    struct tstack_s stack;
    memset(&stack, '\0', sizeof(struct tstack_s));
    stack.root = (s7_pointer)data;

    int rc = mustach_scm_file(template, template_sz,
                              &scm_methods,
                              &stack,
                              _flags,
                              f);

    if (rc) {
        log_error("mustach_scm_frender failure"); // : %d", *err);
    }
    return rc;
}

/* int mustache_scm_fdrender(int fd, */
/*                           const char *template, */
/*                           size_t template_sz, */
/*                           s7_pointer data, */
/*                           int _flags) */
/* { */
/*     return 0; */
/* } */

/* ################################################################ */

/*     else if (port == s7_undefined(s7)) { */
/*         // :port #t - send to current-outp, return str */
/*         // :port () - send to current-outp, do NOT return str */
/*         int rc = mustach_wrap_mem(s7_string(template), */
/*                                   0, // tlength, */
/*                                   &mustach_wrap_itf_scm, */
/*                                   &e, /\* closure, struct tstack_s* *\/ */
/*                                   flags, */
/*                                   &ret, */
/*                                   &size); */
/*         if (rc < 0) { */
/*             log_error("mustach_wrap_mem failure: %s", strerror(errno)); */
/*             return s7_make_integer(s7,rc); */
/*         } else { */
/* #ifdef DEVBUILD */
/*             log_debug("mustach:render wrote %s", ret); */
/* #endif */
/*             s7_pointer s = s7_make_string(s7, ret); */
/*             s7_display(s7, s, s7_current_output_port(s7)); */
/*             if (return_string) { */
/*                 return s; */
/*             } else { */
/*                 return s7_unspecified(s7); */
/*             } */
/*         } */
/*     } */

/*     // (port? port) already verified */
/*     const char *port_filename = s7_port_filename(s7, port); */
/*     (void)port_filename; */
/* #ifdef DEVBUILD */
/*     log_debug("port_filename: %s", port_filename); */
/* #endif */
/*     s7_pointer env = s7_inlet(s7, */
/*                               s7_list(s7, 1, */
/*                                       s7_cons(s7, */
/*                                               s7_make_symbol(s7, "p"), */
/*                                               port))); */
/*     s7_pointer pfile = s7_eval_c_string_with_environment( */
/*                                                          s7, "(port-file p)", */
/*                                                          env); */

/* #ifdef DEVBUILD */
/*     DUMP("port file", pfile); */
/* #endif */
/*     if (s7_c_pointer_type(pfile) == s7_f(s7)) { */
/* #ifdef DEVBUILD */
/*         log_debug("GOT STRING PORT"); */
/* #endif */

/*         int rc = mustach_wrap_mem(s7_string(template), */
/*                                   0, // tlength, */
/*                                   &mustach_wrap_itf_scm, &e, */
/*                                   flags, */
/*                                   &ret, */
/*                                   &size); */
/*         if (rc < 0) { */
/*             log_error("mustach_wrap_mem failure: %s", strerror(errno)); */
/*             return s7_make_integer(s7,rc); */
/*         } else { */
/* #ifdef DEVBUILD */
/*             log_debug("mustach:render wrote %s", ret); */
/* #endif */
/*             s7_display(s7, s7_make_string(s7, ret), port); */
/*             /\* if (return_string) // only true if port = current-output-port *\/ */
/*             /\*     return s7_make_string(s7, ret); *\/ */
/*             /\* else *\/ */
/*             return s7_make_integer(s7, size); */
/*         } */

/*     } else { */
/* #ifdef DEVBUILD */
/*         log_debug("GOT FILE PORT"); */
/* #endif */
/*         /\* (void)data_scheme;          /\\* currently unused *\\/ *\/ */
/*         struct tstack_s e; */
    /* memset(&stack, '\0', sizeof(struct tstack_s)); */
/*         e.root = (s7_pointer)data; */
/*         s7_flush_output_port(s7, s7_current_output_port(s7)); */
/*         int rc = mustach_wrap_file(s7_string(template), 0, // tlength, */
/*                                  &mustach_wrap_itf_scm, &e, */
/*                                  flags, */
/*                                  s7_c_pointer(pfile)); */
/*         (void)rc; */
/* #ifdef DEVBUILD */
/*         log_debug("FILE port RC: %d", rc); */
/* #endif */
/*         /\* cop = s7_current_output_port(s7); *\/ */
/*         /\* os = s7_output_string(s7, cop); *\/ */
/*         /\* const char *os = s7_get_output_string(s7, cop); *\/ */
/*         /\* log_debug("current output str: %s", os); *\/ */
/*         /\* log_debug("UUxxxxxxxxxxxxxxxxx"); *\/ */
/*         /\* (void) os; *\/ */
/*     } */

/*     return s7_make_string(s7, ret); */
