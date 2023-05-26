/* see https://github.com/joeltg/json.scm */
/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "log.h"
#include "mustachios7_s7.h"
#include "trace.h"
/* #include "c_stacktrace.h" */

/* #define CRITICAL(str) \ */
/* 	{print_stacktrace(0); \ */
/* 	fprintf(stderr, "%s (in %s at %s:%i)\n", str, __func__, __FILE__, __LINE__); \ */
/* 	exit(EXIT_FAILURE); \ */
/* 	} */

#ifdef DEBUGGING
#include "ansi_colors.h"
#include "debug.h"
#endif

s7_scheme *s7;

/* one workbuf per lambda #tag, to accumulate content */
int workbufs_tos = 0;               /* top of stack */
struct workbuf_s {
    char buf[4096]; /* big enough to hold (resolved) content between {{#f}} and {{/f}} */
} workbuf_stack[16]; /* nesting - content of lambda tag could contain other lambda tags
                       e.g. {{#f}} ffoo {{#g}} gfoo {{#h}} hfoo {{hitem}} hbar {{/h}} gbar {{/g}} fbar {{/f}}
                       any of f, g, h, and x could be a lambda. #h could contain multiple htimems.
                       so the stack of buffers may not match the stack of hashtags one-to-one.
                     */

static int start(void *closure)
{
    TRACE_ENTRY(start)
    struct tstack_s *e = (struct tstack_s*)closure;
    e->depth = 0;
    s7_pointer x = s7_nil(s7);
    e->selection = x;
    e->stack[0].cont = s7_nil(s7);
    e->stack[0].obj = e->root;
    e->stack[0].index = 0;
    e->stack[0].count = 1;
    fflush(NULL);
    return MUSTACH_OK;
}

static int compare(void *closure, const char *value)
{
#ifdef DEBUGGING
    log_debug("compare");
#endif
    struct tstack_s *e = closure;
    s7_pointer o = e->selection;

    if (s7_is_real(o)) {
        s7_double d = s7_number_to_real(s7, o) - atof(value);
        return d < 0 ? -1 : d > 0 ? 1 : 0;
    } else 	if (s7_is_integer(o)) {
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
#ifdef DEBUGGING
            log_debug("is_proc: %d", s7_is_procedure(cadr));
#endif
            selection = s7_f(s7); /* FIXME: support lambda predicates */
        } else {
            if (e->stack[e->depth].index + 1 == e->stack[e->depth].count) {
                selection = s7_f(s7);
            } else {
                s7_pointer ctx = e->stack[e->depth].cont;
#ifdef DEBUGGING
                TRACE_S7_DUMP("last in ctx?", ctx);
                log_debug("index: %d", e->stack[e->depth].index);
#endif
                // search rest of ctx to see if current key is last
                // I'm sure there is a more efficient way to do this...
                s7_pointer tail;
                if (s7_is_vector(ctx)) {
#ifdef DEBUGGING
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
                s7_pointer ctx = e->stack[e->depth].cont;
#ifdef DEBUGGING
                TRACE_S7_DUMP("last in ctx?", ctx);
                log_debug("index: %d", e->stack[e->depth].index);
#endif
                // search rest of ctx to see if current key is last
                // I'm sure there is a more efficient way to do this...
                s7_pointer tail;
                if (s7_is_vector(ctx)) {
#ifdef DEBUGGING
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
#ifdef DEBUGGING
            log_debug("pred satisfied");
#endif
            selection = s7_t(s7);
        }
    }
    return selection;
}

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

        // special handling: (:a 1 2 3) v. (:a (1 2 3))
        // cdr always a list
        // (cdr (:a 1 2 3)) > (1 2 3)
        // (cdr (:a (1 2 3))) > ((1 2 3))
        // in latter case, (cddr x) = '()
        // but: (:a (1 2) (3 4)) - list of lists
        // then val is ((1 2) (3 4))
#ifdef DEBUGGING
        log_debug("assoc-val len: %d", s7_list_length(s7, assoc_val));
#endif
        if (s7_list_length(s7, assoc_val) == 1) {
            // case: (1)  from (:a 1)
            s7_pointer car = s7_car(assoc_val);
            if (s7_is_list(s7, car)) {
                // case: ((1))
                // case: ((1 2 3))
                // case: ((lambda ...))
                TRACE_S7_DUMP("car is list", car);
                if (car == s7_nil(s7)) {
                    selection = car;
                }
                else if (libs7_is_alist(s7, car)) {
                    TRACE_S7_DUMP("car is alist", car);
                    selection = car;
                } else {
                    selection = assoc_val;
                }
            } else {
                TRACE_S7_DUMP("car is NOT list", car);
                if (s7_is_procedure(car)) {
                    TRACE_LOG_DEBUG("car is lambda", "");
                    if (e->stack[e->depth].index +1 < e->stack[e->depth].count) {
                        e->lambda = true;
                    } else {
                        e->lambda = false;
                        e->stack[e->depth].lambda = false;
                    }
                    e->stack[e->depth].lambda = true;
                    selection = car;
                }
                else if (s7_is_vector(car)) {
#ifdef DEBUGGING
                    log_debug("car is vector");
#endif
                    selection = car;
                } else {
#ifdef DEBUGGING
                    log_debug("car is nonlambda, nonvector");
#endif
                    selection = car;
                }
            }
        } else {
#ifdef DEBUGGING
            log_debug("assoc_val len > 1");
#endif
            selection = assoc_val;
        }
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
#ifdef DEBUGGING
            log_debug("not lambda");
#endif
            selection = assoc_val;
        }
    }
    return selection;
}

/* called by wrap, closure is app struct tstack_s* */
static int sel(void *closure, const char *key)
{
    // key NULL means: select current obj (topmost in stack?)
    TRACE_ENTRY(sel);
    struct tstack_s *e = closure;
    e->stack[e->depth].predicate = e->predicate;

#ifdef DEBUGGING
    log_trace("key: %s", key);
    log_trace("predicate: %d", e->predicate);
    DUMP_CLOSURE(e, e->depth);
#endif
    s7_pointer selection, assoc;
    int i=0, r=0; // r: found? boolean


    if (key == NULL) {
#ifdef DEBUGGING
        log_debug("e->predicate: %d", e->predicate);
        /* log_debug("NULL; final pred: %d", e->final_predicate); */
        /* log_debug("NULL; nonfinal pred: %d", e->nonfinal_predicate); */
#endif
        if (e->predicate) {
            switch(e->predicate) {
            case FIRST_P:
#ifdef DEBUGGING
                log_debug("case: FIRST_P");
#endif
                e->stack[e->depth].predicate = e->predicate;
                if (e->stack[e->depth].index == 0) {
#ifdef DEBUGGING
                    log_debug("predicate is truthy!");
#endif
                    selection = s7_t(s7);
                } else {
#ifdef DEBUGGING
                    log_debug("predicate is false!");
#endif
                    selection = s7_f(s7);
                }
                r = 1;
                break;
            case LAST_P:
#ifdef DEBUGGING
                log_debug("case: LAST_P");
#endif
                e->stack[e->depth].predicate = e->predicate;
                if (e->stack[e->depth].index + 1 < e->stack[e->depth].count) {
#ifdef DEBUGGING
                    log_debug("predicate is false!");
#endif
                    selection = s7_f(s7);
                } else {
#ifdef DEBUGGING
                    log_debug("predicate is truthy!");
#endif
                    selection = s7_t(s7);
                }
                r = 1;
                break;
            case BUTLAST_P:
#ifdef DEBUGGING
                log_debug("case: BUTLAST_P");
#endif
                e->stack[e->depth].predicate = e->predicate;
                // true for all but last
                if (e->stack[e->depth].index + 1 < e->stack[e->depth].count) {
                    /* s7_pointer tmp = s7_vector_ref(s7, e->stack[e->depth].cont, e->stack[e->depth].index); */
                    /* (void)tmp; */
#ifdef DEBUGGING
                    log_debug("predicate is truthy!");
#endif
                    selection = s7_t(s7);
                } else {
#ifdef DEBUGGING
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
            TRACE_S7_DUMP("NULL key, selecting obj", e->stack[e->depth].obj);
            selection = e->stack[e->depth].obj;
            r = 1;
        }
    }
    else if (strcmp(key, "?") == 0) {
        if (e->stack[e->depth].index +1 < e->stack[e->depth].count) {
#ifdef DEBUGGING
            log_debug("ADD SMART COMMA");
#endif
            selection = s7_t(s7);
            r = 1; // sel S_ok?
        } else {
#ifdef DEBUGGING
            log_debug("OMIT SMART COMMA");
#endif
            selection = s7_f(s7);
            r = 0;
        }
    } else if (strcmp(key, "$") == 0) {
        if (e->stack[e->depth].index +1 < e->stack[e->depth].count) {
#ifdef DEBUGGING
            log_debug("ADD SMART COMMA");
#endif
            selection = s7_t(s7);
            r = 1; // sel S_ok?
        } else {
#ifdef DEBUGGING
            log_debug("OMIT SMART COMMA");
#endif
            selection = s7_f(s7);
            r = 0;
        }
    } else {
        i = e->depth;
        // find obj in stack matching key arg
        // FIXME: should we search obj or cont?
        // e->stack[i].obj is an s7_pointer for a json object
        // (so it should be an assoc-list?)
        // to match, key must be a string fld in the obj
        // i.e. must be a key
        // cJSON_GetObjectItemCaseSensitive(obj, key) (cJSON)
        // json_object_object_get_ex (json-c)
        // json_object_get (jansson)
        // scm: (assoc key obj) ???

        //FIXME: use make_symbol even for :foo kw keys

        s7_pointer key_s7;
        if (key[0] == ':') {
            key_s7 = s7_make_symbol(s7, key+1);
        } else {
            key_s7 = s7_make_keyword(s7, key);
        }
        TRACE_S7_DUMP("key_s7", key_s7);
        //FIXME: first search current stackframe (including context), then rest of stack
        // frames below top represent selected vals
        // search should query just that, or the ctx?
#ifdef DEBUGGING
        log_debug("searching stack, height: %d", i);
#endif
        while (i >= 0) {
#ifdef DEBUGGING
            log_debug("i: %d", i);
            /* DUMP_CLOSURE(e, i); */
#endif
            TRACE_S7_DUMP("e->stack[i].obj", e->stack[i].obj);
            TRACE_S7_DUMP("e->stack[i].cont", e->stack[i].cont);

            if (libs7_is_alist(s7, e->stack[i].obj)) { // .cont?
                TRACE_LOG_DEBUG("ALIST", "");
                if (key_s7 == s7_make_keyword(s7, "^")) {
                    log_debug("XXXXXXXXXXXXXXXX");
                } else {
                    assoc = s7_assoc(s7, key_s7, e->stack[i].obj); // .cont?
#ifdef DEBUGGING
                    TRACE_S7_DUMP("assoc result", assoc);
#endif
                    if (assoc != s7_f(s7)) {
#ifdef DEBUGGING
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

                        /* else { */
                        /*     selection = s7_cdr(assoc); */
                        /* } */
                        break;
                    } else {
#ifdef DEBUGGING
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

                if (key_s7 == s7_car(e->stack[i].obj)) {
#ifdef DEBUGGING
                    log_debug("MATCH at stackframe %d", i);
#endif
                    selection = s7_cadr(e->stack[i].obj);
                    break;
                } else {
                    TRACE_S7_DUMP("mismatch on obj", e->stack[i].obj);
                    assoc = s7_assoc(s7, key_s7, e->stack[i].cont);
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
            else if (s7_is_hash_table(e->stack[i].obj)) {
                TRACE_LOG_DEBUG("HASH-TABLE", "");
                selection = s7_hash_table_ref(s7, e->stack[i].obj, key_s7);
                if (selection != s7_f(s7)) {
                    /* log_debug("HIT ONE at %d", i); */
                    break;
                } else {
                    /* log_debug("MISS at %d", i); */
                }

            }
            i--;
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
#ifdef DEBUGGING
    TRACE_S7_DUMP("matched", selection);
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
#ifdef DEBUGGING
    TRACE_S7_DUMP("e->selection", e->selection);
    log_debug("e->predicate: %d", e->predicate);
    log_debug("e->stack[%d].predicate: %d", e->depth, e->stack[e->depth].predicate);
    log_debug("e->stack[%d].lambda: %d", e->depth, e->stack[e->depth].lambda);
    /* DUMP_CLOSURE(e, i); */
#endif
    return r;
}

/* for segments in dotted key, e.g. name of {{person.name}} */
static int subsel(void *closure, const char *name)
{
    TRACE_ENTRY(subsel);
#ifdef DEBUGGING
    log_debug("\tname: '%s'", name);

#endif
    struct tstack_s *e = closure;
#ifdef DEBUGGING
    DUMP_CLOSURE(e, e->depth);
#endif
    s7_pointer o;
    int r = 0;

    /* s7_GetObjectItemCaseSensitive(e->selection, name); */
    /* json_object_object_get_ex */
    /* json_object_get */
    s7_pointer key = s7_make_keyword(s7, name);
    if (s7_is_list(s7, e->selection)) {
        o = s7_assoc(s7, key, e->selection);
        if (o == s7_f(s7)) {
            e->selection = o;
            r = 0;
        } else {
            e->selection = s7_cadr(o);
            r = 1;
        }
    } else if (s7_is_hash_table(e->selection)) {
        o = s7_hash_table_ref(s7,e->selection, key);
        e->selection = o;
        if (o == s7_f(s7)) {
            r = 0;
        } else {
            r = 1;
        }
    } else {
        o = s7_undefined(s7);
    }
    TRACE_S7_DUMP("assoc result", o);
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
#ifdef DEBUGGING
    log_debug("predicate: %d", ((struct closure_hdr*)closure)->predicate);
#endif
    struct tstack_s *e = closure;
    s7_pointer selected;

#ifdef DEBUGGING
    DUMP_CLOSURE(e, e->depth);
#endif

    /* if (!e->lambda) { */
#ifdef DEBUGGING
        log_debug("incrementing stackframe idx from %d", e->depth);
#endif
        e->depth++;
        if (e->depth >= MUSTACH_MAX_DEPTH)
            return MUSTACH_ERROR_TOO_DEEP;
    /* } */
    selected = e->selection;
    e->stack[e->depth].is_objiter = 0;
    if (e->predicate) {
        e->stack[e->depth].predicate = e->predicate;
#ifdef DEBUGGING
        log_debug("selection type: PREDICATE");
        TRACE_S7_DUMP("selected", selected);
        log_debug("w->predicate: %d", e->predicate);
#endif
        if ((selected == s7_unspecified(s7)) || selected == s7_f(s7)) {
            goto not_entering;
        } else { // truthy
            e->stack[e->depth].count = 1;
            e->stack[e->depth].cont = s7_nil(s7);
            e->stack[e->depth].obj = selected;
            e->stack[e->depth].index = 0;
        }
    }
    else if (objiter) { // objiter is boolean?
        if (! s7_is_hash_table(selected) )
            goto not_entering;
        // is ht empty?
        s7_pointer env = s7_inlet(s7,
                                  s7_list(s7, 1,
                                          s7_cons(s7,
                                                  s7_make_symbol(s7, "ht"),
                                                  selected)));
        char *sexp = "(hash-table-entries ht)";
        s7_pointer ht_ct_
            = s7_eval_c_string_with_environment(s7, sexp, env);
        int ht_ct = s7_number_to_integer(s7, ht_ct_);
        /* log_debug("ht ct: %d", ht_ct); */
        if (ht_ct < 1)
            goto not_entering;

        e->stack[e->depth].iter = s7_make_iterator(s7, selected);

        // if obj (alist) is empty, then exit
        // cJSON: child is ptr to chain if items in obj?
        /* if (selected->child == NULL) */
        if ( s7_is_null(s7, selected) )
            goto not_entering;
#ifdef DEBUGGING
        log_debug("OBJITER: json object (hash table)");
#endif
        e->stack[e->depth].cont = selected;
        e->stack[e->depth].obj = s7_car(selected);
        /* e->stack[e->depth].next = s7_cdr(selected); // selected->child->next; */
        e->stack[e->depth].is_objiter = 1;

    }
    else if (s7_is_procedure(selected)) { /* LAMBDA */
#ifdef DEBUGGING
        log_debug("selection type: LAMBDA");
#endif
        e->lambda = true;
        e->stack[e->depth].lambda = true;
        e->stack[e->depth].workbuf_idx = ++workbufs_tos;
        strlcpy(workbuf_stack[workbufs_tos].buf, "Test", 5);
        e->stack[e->depth].count = 1;
        // context is obj of prev. stackframe?
        e->stack[e->depth].cont = e->stack[e->depth-1].obj;
        e->stack[e->depth].obj = selected;
        e->stack[e->depth].index = 0;
    }
    else if (s7_is_vector(selected)) {                         /* VECTOR */
#ifdef DEBUGGING
        log_debug("selection type: VECTOR");
        DUMP("vec", selected);
#endif
        e->stack[e->depth].count = s7_vector_length(selected);
        if (e->stack[e->depth].count == 0)
            goto not_entering;
        e->stack[e->depth].cont = selected;
        e->stack[e->depth].obj = s7_vector_ref(s7, selected, 0);
        e->stack[e->depth].index = 0;

    }
    else if (s7_is_list(s7, selected)) { /* LIST */
#ifdef DEBUGGING
        log_debug("selection type: LIST");
        DUMP("list", selected);
#endif
        if (s7_is_null(s7, selected)) {
#ifdef DEBUGGING
            log_debug("selection type: LIST (NULL)");
#endif
        }
        else if (libs7_is_alist(s7, selected)) { /* ALIST */
#ifdef DEBUGGING
            log_debug("selection type: ALIST");
            log_debug("setting stackframe for selection type: ALIST (NON-NULL)");
#endif
            e->stack[e->depth].count = 1; // single object, not a vector
            if (e->stack[e->depth].count == 0)
                goto not_entering;
            e->stack[e->depth].cont = selected;
            e->stack[e->depth].obj = s7_car(selected);
            e->stack[e->depth].index = 0;

        } else {
#ifdef DEBUGGING
            log_debug("setting stackframe for selection type: LIST (NON-NULL)");
#endif
            // treat list like vector
            e->stack[e->depth].count = s7_list_length(s7, selected);
            if (e->stack[e->depth].count == 0)
                goto not_entering;
            e->stack[e->depth].cont = selected;
            e->stack[e->depth].obj = s7_car(selected);
            e->stack[e->depth].index = 0;
        }
    }
    else if (selected == s7_unspecified(s7)) {
#ifdef DEBUGGING
        log_debug("selection: UNSPECIFIED");
#endif
        e->stack[e->depth].count = 1;
        e->stack[e->depth].cont = selected;
        e->stack[e->depth].obj = selected;
        e->stack[e->depth].index = 0;
        goto not_entering;
    }
    else if (selected == s7_t(s7)) {
#ifdef DEBUGGING
        log_debug("selection: TRUE");
#endif
        if (e->predicate) {
            e->stack[e->depth].predicate = e->predicate;
        } else {
            e->stack[e->depth].count = 1;
            e->stack[e->depth].cont = selected;
            e->stack[e->depth].obj = selected;
            e->stack[e->depth].index = 0;
            /* goto not_entering; */
        }
    }
    else if (selected != s7_f(s7) && !s7_is_null(s7, selected)) {
        // could be boolean #t, number, string, etc.
#ifdef DEBUGGING
        log_debug("selection TRUTHY");
        DUMP("ATOM", selected);
        log_debug("e->lambda? %d", e->lambda);
        log_debug("e->stack[%d].lambda? %d", e->depth, e->stack[e->depth].lambda);
#endif
        e->stack[e->depth].count = 1;
        e->stack[e->depth].cont = s7_nil(s7);
        e->stack[e->depth].obj = selected;
        e->stack[e->depth].index = 0;
    }
    else {
#ifdef DEBUGGING
        log_debug("selection type: OTHER");
#endif
        goto not_entering;
    }
#ifdef DEBUGGING
    DUMP_CLOSURE(e, e->depth);
    log_debug("returning: ENTERED");
#endif
    /* log_debug("RETURNING"); */
    /* DUMP("obj t is ht?", s7_is_hash_table(e->stack[e->depth].obj)); */
    return 1;

 not_entering:
    /* if (!e->lambda) */
    e->depth--;
#ifdef DEBUGGING
    log_debug("NOT ENTERED");
    log_debug("decrement stackframe idx to %d", e->depth);
    DUMP_CLOSURE(e, e->depth);
#endif
    /* log_debug("RETURNING w/o entry"); */
    return 0;
}

/* next: increment index (& set obj) of current stackframe */
static int next(void *closure)
{
    TRACE_ENTRY(next)
	struct tstack_s *e = closure;
	/* s7_pointer o; */
#ifdef DEBUGGING
        DUMP_CLOSURE(e, e->depth);
        /* log_debug("e->depth: %d", e->depth); */
        /* log_debug("e->stack[%d].index: %d", e->depth, e->stack[e->depth].index); */
        /* log_debug("e->stack[%d].count: %d", e->depth, e->stack[e->depth].count); */
#endif

	if (e->depth <= 0) {
            // FIXME: why is this an error?
            log_error("next: depth <= 0: %d", e->depth);
            fflush(NULL);
            /* exit(EXIT_FAILURE); */
            return MUSTACH_ERROR_CLOSING;
        }

	/* o = e->stack[e->depth].next; */
	/* if (o == s7_nil(s7)) */
	/* 	return 0; */
	if (e->stack[e->depth].is_objiter) {
            s7_pointer tmp = s7_iterate(s7, e->stack[e->depth].iter);
            if (s7_iterator_is_at_end(s7, e->stack[e->depth].iter))
                return 0;
            e->stack[e->depth].obj = tmp;
            return 1; // has_next
	}

	e->stack[e->depth].index++;
	if (e->stack[e->depth].index >= e->stack[e->depth].count) {
#ifdef DEBUGGING
            log_debug("incremented index to %d (end of list)", e->stack[e->depth].index);
#endif
            return 0; // !has_next
        }
        if (s7_is_vector(e->stack[e->depth].cont)) {
            e->stack[e->depth].obj = s7_vector_ref(s7, e->stack[e->depth].cont, e->stack[e->depth].index);
        }
        else if (s7_is_list(s7, e->stack[e->depth].cont)) {
            e->stack[e->depth].obj = s7_list_ref(s7, e->stack[e->depth].cont, e->stack[e->depth].index);
        }

#ifdef DEBUGGING
        log_debug("incremented index to %d", e->stack[e->depth].index);
#endif
	return 1; // has_next
}

static int leave(void *closure, struct mustach_sbuf *sbuf)
{
    TRACE_ENTRY(leave)
        struct tstack_s *e = closure;
#ifdef DEBUGGING
    log_debug("current stackframe");
    DUMP_CLOSURE(e, e->depth);
    /* log_debug("decrementing stackframe index from %d", e->depth); */
#endif

    if (e->depth <= 0) {
        log_debug("cXXXXXXXXXXXXXXXX");
        return MUSTACH_ERROR_CLOSING;
    }
    if (e->stack[e->depth].lambda) {
#ifdef DEBUGGING
        log_debug("workbuf tos: %d", workbufs_tos);
        log_debug("lambda workbuf idx: %d", (e->stack[e->depth].workbuf_idx));
        log_debug("lambda workbuf: %s", (workbuf_stack[e->stack[e->depth].workbuf_idx].buf));
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
#ifdef DEBUGGING
    log_debug("decrementing e->depth from %d", e->depth);
#endif
    e->depth--;
    return 0;
}

/* format e->selection it sbuf.value, for printing? */
/* rc 0: no val? */
static int get(void *closure, struct mustach_sbuf *sbuf, int key)
{
    TRACE_ENTRY(get)
    struct tstack_s *e = closure;
    const char *s;

#ifdef DEBUGGING
    log_debug("key: %s", key);
    log_debug("\tsbuf->releasecb: %x", sbuf->releasecb);
    DUMP_CLOSURE(e, e->depth);
#endif
    /* TRACE_S7_DUMP("selection", e->selection); */

    if (key) { // key is boolean?
#ifdef DEBUGGING
        log_debug("objiter?: %d", key);
#endif
        s = e->stack[e->depth].is_objiter
            /* ? e->stack[e->depth].obj->string */
            ? s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                    s7_make_string(s7, "~A"),
                                    e->stack[e->depth].obj))
            : "";
    }
    else if (s7_is_vector(e->selection)) {
        TRACE_LOG_DEBUG("get: selection is vector", "");
        // use s7's format fn to remove meta-notation
        // e.g. '#(1 2 3)' => '1 2 3'
        // but: #(#(1 2) #("a" "b")) - remove quotes
        // ~^ stops at end of sequence
        // ~| stops after (*s7* 'print-length) elements
        char *fmt = "~{~S~^ ~}";
        s = s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                  s7_make_string(s7, fmt),
                                  e->selection));
        /* s = s7_object_to_c_string(s7, e->selection); */
        /* if (s == NULL) return MUSTACH_ERROR_SYSTEM; */
        /* sbuf->freecb = free;    /\* FIXME: why? *\/ */
    }
    else if (s7_is_list(s7, e->selection)) {
        TRACE_LOG_DEBUG("get: selection is list", "");
        // use s7's format fn to remove meta-notation
        // e.g. '(1 2 3)' => '1 2 3'
        // ~^ stops at end of sequence
        // ~| stops after (*s7* 'print-length) elements
        char *fmt = "~{~A~^ ~}";
        s = s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                  s7_make_string(s7, fmt),
                                  e->selection));
        /* s = s7_object_to_c_string(s7, e->selection); */
        /* if (s == NULL) return MUSTACH_ERROR_SYSTEM; */
        /* sbuf->freecb = free;    /\* FIXME: why? *\/ */
    }
    else if (s7_is_string(e->selection)) {
        TRACE_LOG_DEBUG("get: selection is string", "");
        s = s7_string(e->selection);
    }
    else if (s7_is_symbol(e->selection)) {
#ifdef DEBUGGING
        log_debug("get: selection is symbol");
#endif
        s = s7_format(s7, s7_list(s7, 3, s7_f(s7),
                                  s7_make_string(s7, "~A"),
                                  e->selection));
    }
    else if (s7_is_null(s7, e->selection)) {
#ifdef DEBUGGING
        log_debug("get: selection is null");
#endif
        s = "";
    }
    else if (s7_is_unspecified(s7, e->selection)) {
#ifdef DEBUGGING
        log_debug("get: selection is unspecified - (values)?");
#endif
        /* s = ""; */
        s = NULL; // s7_object_to_c_string(s7, e->selection);
        sbuf->value = s;
        /* log_debug("sbuf->value: %s", sbuf->value); */
        return 0;              /* ???? */
    }
    else if (s7_is_procedure(e->selection)) {
#ifdef DEBUGGING
        log_debug("get: selection is procedure");
#endif
        sbuf->lambda = true;
        s7_pointer arity=s7_car(s7_arity(s7, e->selection));
        s7_pointer result;
        if (s7_integer(arity) == 0) {
#ifdef DEBUGGING
            log_debug("ARITY: 0");
#endif
            result = s7_apply_function(s7, e->selection, s7_nil(s7));
        }
        else if (s7_integer(arity) == 1) {
#ifdef DEBUGGING
            log_debug("ARITY: 1");
#endif
            result = s7_make_string(s7, "LAMBDA_RESULT");
            // Do not apply until we hit end tag
            /* if (s7_is_vector(e->stack[e->depth].cont)) { // or list */
            /*     TRACE_S7_DUMP("CTX is vector", e->stack[e->depth].cont); */
            /*     if (e->stack[e->depth].index < e->stack[e->depth].count) { */
            /*         // e->stack[e->depth].obj is current selection? */
            /*         TRACE_S7_DUMP("Applying lambda to arg", e->stack[e->depth].obj); */
            /*         o = s7_apply_function(s7, */
            /*                               cadr, */
            /*                               s7_list(s7, 1, */
            /*                                       e->stack[e->depth].obj)); */
            /*         TRACE_S7_DUMP("applic result", o); */
            /*     } else { */
            /*         log_error("WTF?"); */
            /*         log_error("e->stack[e->depth].index: %d", e->stack[e->depth].index); */
            /*         log_error("e->stack[e->depth].count: %d", e->stack[e->depth].count); */
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
    }
    else if (s7_is_integer(e->selection)) {
        TRACE_S7_DUMP("e->selection is integer", e->selection);
        s = s7_object_to_c_string(s7, e->selection);
        if (s == NULL) return MUSTACH_ERROR_SYSTEM;
        sbuf->freecb = free;
    }     else {
#ifdef DEBUGGING
        log_debug("get: else");
        TRACE_S7_DUMP("e->selection", e->selection);
#endif
        /* s = s7_PrintUnformatted(e->selection); */
        /* s = json_dumps(e->selection, JSON_ENCODE_ANY | JSON_COMPACT); */
        s = s7_object_to_c_string(s7, e->selection);
        if (s == NULL) return MUSTACH_ERROR_SYSTEM;
        sbuf->freecb = free;    /* FIXME: why? */
    }
    sbuf->value = s;
#ifdef DEBUGGING
    /* log_debug("sbuf->value: %s", sbuf->value); */
    log_debug("\tsbuf->releasecb: %x", sbuf->releasecb);
#endif
    return 1;
}

/* **************************************************************** */
void dump_closure(void *closure)
{
    struct tstack_s *e = closure;

    (void)closure;
    int d = e->depth;
    log_debug("DUMP_CLOSURE");
    log_debug("\tpredicate: %d", e->predicate);
    log_debug("\tlambda: %d", e->lambda);
    log_debug("\tdepth: %d", d);
    TRACE_S7_DUMP("\troot", e->root);
    TRACE_S7_DUMP("\tselection", e->selection);
    if (e->stack[d].cont)
        TRACE_S7_DUMP("\te->stack[%d].cont", e->stack[d].cont);
    else
        log_debug("ctx: ?");
    if (e->stack[d].obj)
        TRACE_S7_DUMP("\te->stack[%d].obj", e->stack[d].obj);
    else
        log_debug("obj: ?");
    log_debug("\te->stack[%d].count: %d", d, e->stack[d].count);
    log_debug("\te->stack[%d].index: %d", d, e->stack[d].index);
    log_debug("\te->stack[%d].lambda: %d", d, e->stack[d].lambda);
    log_debug("\te->stack[%d].predicate: %d", d, e->stack[d].predicate);
    log_debug("end closure");
    fflush(NULL);
}

const struct mustach_wrap_itf mustach_wrap_itf_scm = {
	.start = start,
	.stop = NULL,
	.compare = compare,
	.sel = sel,
	.subsel = subsel,
	.enter = enter,
	.next = next,
	.leave = leave,
	.get = get,
        .dump_closure = dump_closure
};
