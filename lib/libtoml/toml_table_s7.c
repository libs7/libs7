#include <stdlib.h>

#include "libtoml_s7.h"
#include "toml_table_s7.h"
#include "toml_array_s7.h"

int toml_table_type_tag = 0;

s7_pointer string_string;

typedef s7_pointer (*Toml_table_fn)   (s7_scheme*, s7_pointer);

static s7_pointer free_toml_table(s7_scheme *s7, s7_pointer obj)
{
    TRACE_ENTRY(free_toml_table);
    free(s7_c_object_value(obj));
    return(NULL);
}

static s7_pointer mark_toml_table(s7_scheme *s7, s7_pointer obj)
{
  /* toml_table_t *t = (toml_table_t*)s7_c_object_value(obj); */
  /* s7_mark(o->data); */
  return(NULL);
}

/* static */ s7_pointer is_toml_table(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(is_toml_table);
    /* log_debug("c obj?: %d", s7_is_c_object(s7_car(args))); */
    /* log_debug("c obj typ: %d", s7_c_object_type(s7_car(args))); */
    /* log_debug("toml-table typ: %d", toml_table_type_tag); */
  return(s7_make_boolean(s7,
		 s7_is_c_object(s7_car(args)) &&
                 s7_c_object_type(s7_car(args)) == toml_table_type_tag));
}

static s7_pointer toml_table_is_equal(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_is_equal);
    return s7_nil(s7);
}

static s7_pointer toml_table_is_equivalent(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_is_equivalent);
    return s7_nil(s7);
}

s7_pointer toml_table_ref(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_toml_table_ref);
    s7_pointer p, arg;
    toml_table_t* table;
    char* key;
    p = args;
    arg = s7_car(p);
    /* table = (toml_table_t*)s7_c_pointer_with_type(s7, arg, toml_table_t__symbol, __func__, 1); */
    // extract the c pointer
    /* table = (toml_table_t*)s7_c_pointer(arg); */
    table = (toml_table_t*)s7_c_object_value(arg);

    p = s7_cdr(p);
    arg = s7_car(p);
    if (s7_is_string(arg))
        key = (char*)s7_string(arg);
    else return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:table-ref", 14), 2, arg, string_string));

    toml_datum_t datum;

    datum = toml_string_in(table, key);
    if (datum.ok) {
        s7_pointer s = s7_make_string(s7, datum.u.s);
        free(datum.u.s);
        return s;
    }

    datum = toml_bool_in(table, key);
    if (datum.ok) { return(s7_make_boolean(s7, datum.u.b)); }

    datum = toml_int_in(table, key);
    if (datum.ok) { return(s7_make_integer(s7, datum.u.i)); }

    datum = toml_double_in(table, key);
    if (datum.ok) { return(s7_make_real(s7, datum.u.d)); }

    datum = toml_timestamp_in(table, key);
    if (datum.ok) {
        /* not yet supported */
        return(s7_f(s7));
    }

    toml_array_t *a = toml_array_in(table, key);
    if (a) {
        /* log_debug("array"); */
        s7_pointer rval = s7_make_c_object(s7,
                                           toml_array_type_tag,
                                           (void*)a);
        return rval;
        /* return(s7_make_c_pointer_with_type(s7, (void*)array_ptr, toml_array_t__symbol, s7_f(s7))); */
    } else {
        /* log_debug("not array"); */
    }

    toml_table_t *t = toml_table_in(table, key);
    if (t) {
        /* log_debug("table: %p", t); */
        s7_pointer rval = s7_make_c_object(s7,
                                           toml_table_type_tag,
                                           (void*)t);
        /* void *optr = (void*)s7_c_object_value(rval); */
        /* void *optr = (void*)s7_c_object_value_checked(rval, toml_table_type_tag); */
        /* log_debug("rval ptr: %p", rval); */
        /* log_debug("rval objptr: %p", optr); */

        return rval;
    } else {
        /* log_debug("not table"); */
    }

    log_debug("returning #f");
    return(s7_f(s7));
}

/* { */
/*     TRACE_ENTRY(toml_table_ref); */
/*     s7_pointer p, arg; */
/*     toml_table_t* table; */
/*     char* key; */
/*     p = args; */
/*     arg = s7_car(p); */
/*     /\* table = (toml_table_t*)s7_c_pointer_with_type(s7, arg, toml_table_t__symbol, __func__, 1); *\/ */
/*     // extract the c pointer */
/*     /\* table = (toml_table_t*)s7_c_pointer(arg); *\/ */
/*     table = (toml_table_t*)s7_c_object_value(arg); */

/*     p = s7_cdr(p); */
/*     arg = s7_car(p); */
/*     if (arg == NULL) { */
/*         // user passed uninitialized s7_pointer var? */
/*         // even with this we get a segfault */
/*         return s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"), */
/*                  s7_list(s7, 3, s7_make_string(s7, "uninitialized s7_pointer?"))); */
/*     } */
/*     if (s7_is_string(arg)) { */
/*         key = (char*)s7_string(arg); */
/*     } else { */
/*         return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:table-ref", 14), 2, arg, string_string)); */
/*     } */

/*     toml_datum_t datum; */

/*     datum = toml_string_in(table, key); */
/*     if (datum.ok) { */
/*         s7_pointer s = s7_make_string(s7, datum.u.s); */
/*         free(datum.u.s); */
/*         return s; */
/*     } */

/*     datum = toml_bool_in(table, key); */
/*     if (datum.ok) { return(s7_make_boolean(s7, datum.u.b)); } */

/*     datum = toml_int_in(table, key); */
/*     if (datum.ok) { return(s7_make_integer(s7, datum.u.i)); } */

/*     datum = toml_double_in(table, key); */
/*     if (datum.ok) { return(s7_make_real(s7, datum.u.d)); } */

/*     datum = toml_timestamp_in(table, key); */
/*     if (datum.ok) { */
/*         /\* not yet supported *\/ */
/*         return(s7_f(s7)); */
/*     } */

/*     toml_array_t *a = toml_array_in(table, key); */
/*     if (a) { */
/*         log_debug("array"); */
/*         s7_pointer rval = s7_make_c_object(s7, toml_array_type_tag, (void*)a); */
/*         return rval; */
/*         /\* return(s7_make_c_pointer_with_type(s7, (void*)array_ptr, toml_array_t__symbol, s7_f(s7))); *\/ */
/*     } else { */
/*         log_debug("not array"); */
/*     } */

/*     toml_table_t *t = toml_table_in(table, key); */
/*     if (t) { */
/*         s7_pointer rval = s7_make_c_object(s7, toml_table_type_tag, (void*)t); */
/*         return rval; */
/*         /\* return(s7_make_c_pointer_with_type(s7, (void*)table_val, toml_array_t__symbol, s7_f(s7))); *\/ */
/*     } */

/*     return(s7_f(s7)); */
/* } */

static s7_pointer toml_table_set(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_set);
    return s7_nil(s7);
}

s7_pointer toml_table_length(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_length);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);

    void *t = (void*)s7_c_object_value_checked(arg, toml_table_type_tag);
    if (t) {
        int ntab = toml_table_ntab(t);
        int narr = toml_table_narr(t);
        int nkv = toml_table_nkval(t);
        s7_pointer i = s7_make_integer(s7, ntab + narr + nkv);
        return(i);
    } else {
        log_error("Bad arg, expected table, actual: %d", s7_c_object_type(arg));
        //FIXME: throw error
        return(s7_unspecified(s7));
    }
}

static s7_pointer toml_table_copy(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_set);
    return s7_nil(s7);
}

static s7_pointer toml_table_fill(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_set);
    return s7_nil(s7);
}

static s7_pointer toml_table_reverse(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_set);
    return s7_nil(s7);
}

static s7_pointer toml_table_to_list(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_to_list);
    return s7_nil(s7);
}

static s7_pointer toml_table_to_string(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_to_string);
    return s7_nil(s7);
}

static s7_pointer toml_table_getter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_getter);
    return s7_nil(s7);
}

static s7_pointer toml_table_setter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_setter);
    return s7_nil(s7);
}

void toml_table_init(s7_scheme *s7)
{
    TRACE_ENTRY(toml_table_init);
    toml_table_type_tag = s7_make_c_type(s7, "toml_table");
    /* log_debug("toml_table_type_tag: %d", toml_table_type_tag); */

    s7_c_type_set_gc_free      (s7, toml_table_type_tag, free_toml_table);
    s7_c_type_set_gc_mark      (s7, toml_table_type_tag, mark_toml_table);
    s7_c_type_set_is_equal     (s7, toml_table_type_tag, toml_table_is_equal);
    s7_c_type_set_is_equivalent(s7, toml_table_type_tag, toml_table_is_equivalent);
    s7_c_type_set_ref          (s7, toml_table_type_tag, toml_table_ref);
    s7_c_type_set_set          (s7, toml_table_type_tag, toml_table_set);
    s7_c_type_set_length       (s7, toml_table_type_tag, toml_table_length);
    s7_c_type_set_copy         (s7, toml_table_type_tag, toml_table_copy);
    s7_c_type_set_fill         (s7, toml_table_type_tag, toml_table_fill);
    s7_c_type_set_reverse      (s7, toml_table_type_tag, toml_table_reverse);
    s7_c_type_set_to_list      (s7, toml_table_type_tag, toml_table_to_list);
    s7_c_type_set_to_string    (s7, toml_table_type_tag, toml_table_to_string);

    s7_define_function(s7, "toml:table-getter",
                       toml_table_getter, 2, 0, false,
                       "(toml:table-getter t k) gets value for key k from table t");
    s7_c_type_set_getter       (s7, toml_table_type_tag, s7_name_to_value(s7, "toml:table-getter"));

    s7_define_function(s7, "toml:table-setter",
                       toml_table_setter, 2, 0, false,
                       "(toml:table-setter t k) sets value for key k from table t");
    s7_c_type_set_setter       (s7, toml_table_type_tag, s7_name_to_value(s7, "toml:table-setter"));

    s7_define_function(s7, "toml:table?", is_toml_table, 1, 0, false,
                       "(toml:table? t) returns #t if its argument is a toml_table object");

    string_string = s7_make_semipermanent_string(s7, "a string");
}
