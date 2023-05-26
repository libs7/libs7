#include "gopt.h"
#include "log.h"
#include "unity.h"
#include "utarray.h"
#include "utstring.h"
#include "trace.h"

#include "common.h"

#include "libs7.h"

s7_scheme *s7;

extern struct option options[];

s7_pointer root, jo, k, keys, ja, idx;
s7_pointer flag, res, actual, expected;
s7_pointer len, m, map, v, vec;

char *expected_str;

bool verbose;
bool debug;

char *cmd;

#define DQ "\\\""

#define S(s) "\\\"" #s "\\\""

#define JSON_READ(s) \
    s7_apply_function(s7, s7_name_to_value(s7, "json:read"),    \
                      s7_list(s7, 1, \
                              s7_eval_c_string(s7, "\"" s "\"")));
                              /* s7_eval_c_string(s7, s))); */

#define APPLY_1(f, o) \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                       s7_list(s7, 1, o))

#define APPLY_MAP(m, o) \
    s7_apply_function(s7, m, s7_list(s7, 1, o))

#define APPLY_2(f, o, k)                             \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                   s7_list(s7, 2, o, k))

    /* s7_apply_function_star(s7, s7_name_to_value(s7, f), \ */
    /*                            s7_list(s7, 1, v)) */

/* WARNING: setUp and tearDown are run once per test. */
void setUp(void) {
    /* log_info("setup"); */
}

void tearDown(void) {
    /* log_info("teardown"); */
}

/* (define tlt (json:read "v = [0, 1, 2]")) */
/* tlt is (a nameless) object, NOT a kv pair!  */
void root_object(void) {
    jo = JSON_READ("{" S(m) ": true}");
    actual = APPLY_1("json:map?", jo);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);

    k = s7_make_string(s7, "m");
    flag = APPLY_2("json:map-ref", jo, k);
    actual = APPLY_1("boolean?", flag);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
}

void atomic_types(void) {
    // integer
    jo = JSON_READ("{" S(m) ": 123}");
    actual = APPLY_1("json:map?", jo);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    k = s7_make_string(s7, "m");
    res = APPLY_2("json:map-ref", jo, k);
    flag = APPLY_1("number?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("real?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // float
    jo = JSON_READ("{" S(m) ": 1.23}");
    actual = APPLY_1("json:map?", jo);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    k = s7_make_string(s7, "m");
    res = APPLY_2("json:map-ref", jo, k);
    flag = APPLY_1("number?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("real?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TRACE_S7_DUMP("nbr", res);

    // bad string (single quotes), throws error
    /* jo = JSON_READ("{" S(m) ": 'hello'}"); */

    // string
    jo = JSON_READ("{" S(m) ": " DQ "hello" DQ "}");
    flag = APPLY_1("json:map?", jo);
    TRACE_S7_DUMP("flag", flag);
    TEST_ASSERT_EQUAL(s7_t(s7), actual);
    k = s7_make_string(s7, "m");
    res = APPLY_2("json:map-ref", jo, k);
    TRACE_S7_DUMP("res", res);
    /* flag = APPLY_1("string?", res); */
    /* TRACE_S7_DUMP("flag", flag); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */
    /* TRACE_S7_DUMP("res", res); */

    /* jo = JSON_READ("\"m = \\\"Hello\\\"\")"); */
    /* actual = APPLY_1("json:map?", jo); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), actual); */
    /* k = s7_make_string(s7, "m"); */
    /* res = APPLY_2("json:map-ref", jo, k); */
    /* actual = APPLY_1("string?", res); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), actual); */

    /* jo = JSON_READ("\"m = [1, 2, 3]\")"); */
    /* actual = APPLY_1("json:map?", jo); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), actual); */
    /* k = s7_make_string(s7, "m"); */
    /* res = APPLY_2("json:map-ref", jo, k); */
    /* actual = APPLY_1("json:array?", res); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), actual); */

    /* jo = JSON_READ("\"m = { a = 1, b = 2}\")"); */
    /* actual = APPLY_1("json:map?", jo); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), actual); */
    /* k = s7_make_string(s7, "m"); */
    /* res = APPLY_2("json:map-ref", jo, k); */
    /* actual = APPLY_1("json:map?", res); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), actual); */
}

/*
 * map ops: keys, length, apply, contains?
 */
void map_ops(void) {
    root = JSON_READ("{"
                   S(m) ": "
                   "{"
                   S(b) ":true,"
                   S(s) ": \\\"Hello!\\\", "
                   S(i) ": 0,"
                   S(f) ": 1.2, "
                   S(subm) ": {" S(m1) ": 1 },"
                   S(v) ": [0, 1, 2]"
                   "}}");
    /* flag = APPLY_1("json:map?", root); */
    /* TRACE_S7_DUMP("root", root); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    /* keys = APPLY_1("json:map-keys", root); */
    /* flag = APPLY_1("list?", keys); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    len = APPLY_1("json:map-length", root);
    flag = APPLY_1("integer?", len);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(1, s7_integer(len));

    k = s7_make_string(s7, "m");
    m = APPLY_2("json:map-ref", root, k);
    /* TRACE_S7_DUMP("m", m); */
    flag = APPLY_1("json:map?", m);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    keys = APPLY_1("json:map-keys", m);
    TRACE_S7_DUMP("keys", keys);
    flag = APPLY_1("list?", keys);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    k = s7_make_string(s7, "b");
    flag = APPLY_2("json:map-contains?", m, k);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    k = s7_make_string(s7, "foobar");
    flag = APPLY_2("json:map-contains?", m, k);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    k = s7_make_string(s7, "v");
    flag = APPLY_2("json:map-contains?", m, k);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    vec = APPLY_MAP(m, k);
    /* vec = APPLY_2("json:map-ref", m, k); */
    TRACE_S7_DUMP("vec", vec);
    flag = APPLY_1("json:array?", vec);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    //FIXME: test json:map-key-for-index ('key_in')
}

void apply_map(void) {
    jo = JSON_READ("{" S(m) ": 123}");
    flag = APPLY_1("json:map?", jo);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // apply json map to key
    k = s7_make_string(s7, "m");
    res = s7_apply_function(s7, jo, s7_list(s7, 1, k));
    flag = APPLY_1("number?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("real?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
}

void map_refs(void) {
    root = JSON_READ("{" S(m) ": 123}");
    flag = APPLY_1("json:map?", root);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    k = s7_make_string(s7, "m");
    res = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:array?", res);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL_INT(123, s7_integer(res));

    k = s7_make_symbol(s7, "m");
    res = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:array?", res);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL_INT(123, s7_integer(res));

    k = s7_make_keyword(s7, "m");
    res = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:array?", res);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL_INT(123, s7_integer(res));

    /* /\* apply object to key: (t k) == (json:map-ref t k) *\/ */
    /* a = s7_apply_function(s7, t, s7_list(s7, 1, k)); */
    /* flag = APPLY_1("json:array?", a); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */

    // try hash-object-ref - nope, segfault
    /* a = APPLY_2("hash-object-ref", t, k); */
    /* flag = APPLY_1("json:array?", a); */
    /* TEST_ASSERT_EQUAL(s7_t(s7), flag); */
}

/* /\* */
/*  * WARNING: jsonc99 object count ops are typed: */
/*  * ntab for object-valued entries, narr for array-valued entries, */
/*  * and nkv for "key-values", i.e. entries with 'atomic' values */
/*  * so json:map-length must sum the three: ntab + narr + nkv */
/*  *\/ */
/* void object_length_ops(void) { */
/*     char *json = "" */
/*         "\"m = { b = true, s = \\\"Hello!\\\", " */
/*         "        i = 0, f = 1.2, " */
/*         "        jo = { t1 = 1 }, a = [0, 1, 2] }\""; */
/*     jo = JSON_READ(json); */
/*     flag = APPLY_1("json:map?", jo); */
/*     /\* TEST_ASSERT_EQUAL(s7_t(s7), flag); *\/ */

/*     k = s7_make_string(s7, "m"); */
/*     s7_pointer m = APPLY_2("json:map-ref", t, k); */
/*     flag = APPLY_1("json:map?", m); */
/*     /\* TEST_ASSERT_EQUAL(s7_t(s7), flag); *\/ */

/*     actual = APPLY_1("json:map-length", m); */
/*     TEST_ASSERT_EQUAL(6, s7_integer(actual)); */
/* } */

/* void object_serialization(void) { */
/*     /\* jo = JSON_READ("\"m = [1, 2, 3]\")"); *\/ */
/*     /\* jo = JSON_READ("\"k1 = 1\nk2 = true\nk3='Hello'\")"); *\/ */

/*     jo = JSON_READ("\"k1 = 7\nk2 = 8\")"); */

/*     /\* actual = APPLY_FORMAT("\"~A\"", jo); *\/ */
/*     res = s7_apply_function(s7, s7_name_to_value(s7, "object->string"), */
/*                             s7_list(s7, 1, jo)); */
/*     TRACE_S7_DUMP("obj->s", res); */

/*     /\* res = s7_apply_function(s7, s7_name_to_value(s7, "format"), *\/ */
/*     /\*                         s7_list(s7, 3, *\/ */
/*     /\*                                 s7_t(s7), *\/ */
/*     /\*                                 s7_make_string(s7, "~A"), *\/ */
/*     /\*                                 jo)); *\/ */
/*     /\* log_debug("xxxxxxxxxxxxxxxx"); *\/ */
/*     /\* TRACE_S7_DUMP("fmt", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL(s7_t(s7), actual); *\/ */

/*     /\* /\\* root objects have empty key  *\\/ *\/ */
/*     /\* actual = APPLY_1("json:map-key", jo); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("", s7_string(actual)); *\/ */

/*     /\* k = s7_make_string(s7, "m"); *\/ */
/*     /\* s7_pointer m = APPLY_2("json:map-ref", t, k); *\/ */
/*     /\* actual = APPLY_1("json:map?", m); *\/ */
/*     /\* TEST_ASSERT_EQUAL(s7_t(s7), actual); *\/ */

/*     /\* actual = APPLY_1("json:map-key", m); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("m", s7_string(actual)); *\/ */
/*     /\* actual = APPLY_1("json:map-keys", m); *\/ */
/*     /\* s7_pointer v = APPLY_1("list?", actual); *\/ */
/*     /\* TEST_ASSERT_EQUAL(s7_t(s7), v); *\/ */

/*     /\* //FIXME: test json:map-key-for-index ('key_in') *\/ */
/* } */

/* void to_string_atoms(void) { */
/*     // bools */
/*     jo = JSON_READ("\"k1 = true\nk2 = false\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object k1 = true, k2 = false>", */
/*                              s7_string(res)); */
/*     // doubles */
/*     jo = JSON_READ("\"k1 = 1.2\nk2 = 3.4\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object k1 = 1.2, k2 = 3.4>", */
/*                              s7_string(res)); */
/*     // ints */
/*     jo = JSON_READ("\"k1 = 7\nk2 = 8\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object k1 = 7, k2 = 8>", */
/*                              s7_string(res)); */

/*     // strings */
/*     jo = JSON_READ("\"k1 = 'Hi there'\nk2 = ', World'\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object k1 = 'Hi there', k2 = ', World'>", */
/*                              s7_string(res)); */

/*     /\* // timestamps (not yejo) *\/ */
/*     /\* jo = JSON_READ("\"k1 = 'Hi there'\nk2 = ', World'\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TRACE_S7_DUMP("obj->s", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object k1 = 'Hi there', k2 = ', World'>", *\/ */
/*     /\*                          s7_string(res)); *\/ */
/* } */

/* void to_string_arrays(void) { */
/*     // bool arrays */
/*     jo = JSON_READ("\"ba = [true, false]\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TRACE_S7_DUMP("obj->s", res); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object ba = [true, false]>", */
/*                              s7_string(res)); */
/*     // int arrays */
/*     jo = JSON_READ("\"ia = [0, 1, 2]\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object ia = [0, 1, 2]>", */
/*                              s7_string(res)); */
/*     // double arrays */
/*     jo = JSON_READ("\"da = [1.2, 3.4]\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TRACE_S7_DUMP("obj->s", res); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object da = [1.2, 3.4]>", */
/*                              s7_string(res)); */

/*     // string arrays */
/*     jo = JSON_READ("\"sa = ['Hey there', 'you old world']\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TRACE_S7_DUMP("obj->s", res); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object sa = [\"Hey there\", \"you old world\"]>", */
/*                              s7_string(res)); */

/*     /\* // timestamp arrays (not yejo) *\/ */
/*     /\* jo = JSON_READ("\"k1 = 'Hi there'\nk2 = ', World'\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TRACE_S7_DUMP("obj->s", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object k1 = \"Hi there\", k2 = \", World\">", *\/ */
/*     /\*                          s7_string(res)); *\/ */
/* } */

/* void to_string_subobjects(void) { */
/*     // bool values */
/*     jo = JSON_READ("\"a1 = { a1b1 = true }\na2 = 9\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TRACE_S7_DUMP("obj->s", res); */
/*     TEST_ASSERT_EQUAL_STRING("<#json-object a2 = 9, a1 = <#json-object a1b1 = true>>", */
/*                              s7_string(res)); */
/*     /\* // int arrays *\/ */
/*     /\* jo = JSON_READ("\"ia = [0, 1, 2]\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object ia = [0, 1, 2]>", *\/ */
/*     /\*                          s7_string(res)); *\/ */
/*     /\* // double arrays *\/ */
/*     /\* jo = JSON_READ("\"da = [1.2, 3.4]\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TRACE_S7_DUMP("obj->s", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object da = [1.2, 3.4]>", *\/ */
/*     /\*                          s7_string(res)); *\/ */

/*     /\* // string arrays *\/ */
/*     /\* jo = JSON_READ("\"sa = ['Hey there', 'you old world']\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TRACE_S7_DUMP("obj->s", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object sa = ['Hey there', 'you old world']>", *\/ */
/*     /\*                          s7_string(res)); *\/ */

/*     /\* // timestamp arrays (not yejo) *\/ */
/*     /\* jo = JSON_READ("\"k1 = 'Hi there'\nk2 = ', World'\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TRACE_S7_DUMP("obj->s", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object k1 = 'Hi there', k2 = ', World'>", *\/ */
/*     /\*                          s7_string(res)); *\/ */
/* } */

/* void to_string_mixed(void) { */
/*     jo = JSON_READ("\"a1 = [ {a1b1 = [1, 2] } ]\")"); */
/*     res = APPLY_1("object->string", jo); */
/*     TRACE_S7_DUMP("obj->s", res); */
/*     expected_str = "<#json-object a1 = [<#json-object a1b1 = [1, 2]>]>"; */
/*     TEST_ASSERT_EQUAL_STRING(expected_str, s7_string(res)); */

/*     /\* jo = JSON_READ("\"a1 = { a1b1 = [1, 2] }\na2 = [{a2b1=true},{a2b2=99}]\")"); *\/ */


/*     /\* // int arrays *\/ */
/*     /\* jo = JSON_READ("\"ia = [0, 1, 2]\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object ia = [0, 1, 2]>", *\/ */
/*     /\*                          s7_string(res)); *\/ */
/*     /\* // double arrays *\/ */
/*     /\* jo = JSON_READ("\"da = [1.2, 3.4]\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TRACE_S7_DUMP("obj->s", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object da = [1.2, 3.4]>", *\/ */
/*     /\*                          s7_string(res)); *\/ */

/*     /\* // string arrays *\/ */
/*     /\* jo = JSON_READ("\"sa = ['Hey there', 'you old world']\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TRACE_S7_DUMP("obj->s", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object sa = ['Hey there', 'you old world']>", *\/ */
/*     /\*                          s7_string(res)); *\/ */

/*     /\* // timestamp arrays (not yejo) *\/ */
/*     /\* jo = JSON_READ("\"k1 = 'Hi there'\nk2 = ', World'\")"); *\/ */
/*     /\* res = APPLY_1("object->string", jo); *\/ */
/*     /\* TRACE_S7_DUMP("obj->s", res); *\/ */
/*     /\* TEST_ASSERT_EQUAL_STRING("<#json-object k1 = 'Hi there', k2 = ', World'>", *\/ */
/*     /\*                          s7_string(res)); *\/ */
/* } */

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "json");

    UNITY_BEGIN();

    /* RUN_TEST(root_object); */
    /* RUN_TEST(atomic_types); */
    /* RUN_TEST(map_ops); */
    /* RUN_TEST(apply_map); */
    RUN_TEST(map_refs);
    /* RUN_TEST(object_length_ops); */
    /* RUN_TEST(object_serialization); */

    /* RUN_TEST(to_string_atoms); */
    /* RUN_TEST(to_string_arrays); */
    /* RUN_TEST(to_string_arrays); */
    /* RUN_TEST(to_string_subobjects); */
    /* RUN_TEST(to_string_mixed); */

    /* RUN_TEST(dotted_keys); */

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
