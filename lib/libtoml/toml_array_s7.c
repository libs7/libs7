#include <stdlib.h>

#include "toml_array_s7.h"

int toml_array_type_tag = 0;

static s7_pointer free_toml_array(s7_scheme *s7, s7_pointer obj)
{
    TRACE_ENTRY(free_toml_array);
    free(s7_c_object_value(obj));
    return(NULL);
}

static s7_pointer mark_toml_array(s7_scheme *s7, s7_pointer obj)
{
  /* toml_array_t *t = (toml_array_t*)s7_c_object_value(obj); */
  /* s7_mark(o->data); */
  return(NULL);
}

/* static */ s7_pointer is_toml_array(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(is_toml_array);
    log_debug("c obj?: %d", s7_is_c_object(s7_car(args)));
    log_debug("c obj typ: %d", s7_c_object_type(s7_car(args)));
    log_debug("toml-array typ: %d", toml_array_type_tag);
  return(s7_make_boolean(s7,
		 s7_is_c_object(s7_car(args)) &&
                 s7_c_object_type(s7_car(args)) == toml_array_type_tag));
}

static s7_pointer toml_array_is_equal(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_is_equal);
    return s7_nil(s7);
}

static s7_pointer toml_array_is_equivalent(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_is_equivalent);
    return s7_nil(s7);
}

static s7_pointer toml_array_ref(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_ref);
    return s7_nil(s7);
}

static s7_pointer toml_array_set(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    return s7_nil(s7);
}

static s7_pointer toml_array_length(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    return s7_nil(s7);
}

static s7_pointer toml_array_copy(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    return s7_nil(s7);
}

static s7_pointer toml_array_fill(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    return s7_nil(s7);
}

static s7_pointer toml_array_reverse(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    return s7_nil(s7);
}

static s7_pointer toml_array_to_list(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_to_list);
    return s7_nil(s7);
}

static s7_pointer toml_array_to_string(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_to_string);
    return s7_nil(s7);
}

static s7_pointer toml_array_getter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_getter);
    return s7_nil(s7);
}

static s7_pointer toml_array_setter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_setter);
    return s7_nil(s7);
}

void toml_array_init(s7_scheme *s7)
{
    TRACE_ENTRY(toml_array_init);
    toml_array_type_tag = s7_make_c_type(s7, "toml_array");
    /* log_debug("toml_array_type_tag: %d", toml_array_type_tag); */

    s7_c_type_set_gc_free      (s7, toml_array_type_tag, free_toml_array);
    s7_c_type_set_gc_mark      (s7, toml_array_type_tag, mark_toml_array);
    s7_c_type_set_is_equal     (s7, toml_array_type_tag, toml_array_is_equal);
    s7_c_type_set_is_equivalent(s7, toml_array_type_tag, toml_array_is_equivalent);
    s7_c_type_set_ref          (s7, toml_array_type_tag, toml_array_ref);
    s7_c_type_set_set          (s7, toml_array_type_tag, toml_array_set);
    s7_c_type_set_length       (s7, toml_array_type_tag, toml_array_length);
    s7_c_type_set_copy         (s7, toml_array_type_tag, toml_array_copy);
    s7_c_type_set_fill         (s7, toml_array_type_tag, toml_array_fill);
    s7_c_type_set_reverse      (s7, toml_array_type_tag, toml_array_reverse);
    s7_c_type_set_to_list      (s7, toml_array_type_tag, toml_array_to_list);
    s7_c_type_set_to_string    (s7, toml_array_type_tag, toml_array_to_string);

    s7_define_function(s7, "toml:array-getter",
                       toml_array_getter, 2, 0, false,
                       "(toml:array-getter t k) gets value for key k from array t");
    s7_c_type_set_getter       (s7, toml_array_type_tag, s7_name_to_value(s7, "toml:array-getter"));

    s7_define_function(s7, "toml:array-setter",
                       toml_array_setter, 2, 0, false,
                       "(toml:array-setter t k) sets value for key k from array t");
    s7_c_type_set_setter       (s7, toml_array_type_tag, s7_name_to_value(s7, "toml:array-setter"));

    s7_define_function(s7, "toml:array?", is_toml_array, 1, 0, false,
                       "(toml:array? t) returns #t if its argument is a toml_array object");
}
