#ifndef TEST_COMMON_H
#define TEST_COMMON_H

#include "gopt.h"
#include "libs7.h"

void print_usage(char *test);

void set_options(char *test, struct option options[]);

void print_debug_env(void);

s7_scheme *initialize(char *test, int argc, char **argv);

#define DQ3 "\"\"\""
#define SQ3 "'''"

//FIXME: store json:read in a var
#define JSON_READ(s) \
    s7_apply_function(s7, s7_name_to_value(s7, "json:read"),    \
                      s7_list(s7, 1,                            \
                      s7_make_string(s7, s)));
                      /* s7_eval_c_string(s7, "\"" s "\""))); */

#define TOML_READ(s) \
    s7_apply_function(s7, s7_name_to_value(s7, "toml:read"),    \
                      s7_list(s7, 1,                            \
                      s7_make_string(s7, s)));

                      /* s7_eval_c_string(s7, "\"" s "\""))); */

#define SEXP_READ(s) \
    s7_apply_function(s7, s7_name_to_value(s7, "sexp:read"),    \
                      s7_list(s7, 1,                            \
                      s7_make_string(s7, s)));

                      /* s7_eval_c_string(s7, "\"" s "\""))); */


#define EVAL(s) s7_eval_c_string(s7, s)

#define APPLY_OBJ(obj, arg)                            \
    s7_apply_function(s7, obj, s7_list(s7, 1, arg))

#define APPLY_1(f, o) \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                       s7_list(s7, 1, o))

#define APPLY_2(f, o, k)                             \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                   s7_list(s7, 2, o, k))

#define APPLY_3(f, a, b, c)                          \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                   s7_list(s7, 3, a, b, c))


#endif // TEST_COMMON_H
