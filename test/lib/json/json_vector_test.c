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
s7_pointer len, m, map, v, vec, vec2;

char *expected_str;

bool verbose;
bool debug;

char *cmd;

#define DQ "\\\""

#define S(s) "\\\"" #s "\\\""

#define APPLY_1(f, o) \
 s7_apply_function(s7, s7_name_to_value(s7, f),    \
                       s7_list(s7, 1, o))

#define APPLY_OBJ(obj, arg) \
    s7_apply_function(s7, obj, s7_list(s7, 1, arg))

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

/*
 * vector ops: vector?, ref, length, apply
 */
void int_vector_ops(void) {
    root = JSON_READ("{" S(v) ": [10, 11, 12] }");
    flag = APPLY_1("json:map?", root);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("json:vector?", root);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    k = s7_make_string(s7, "v");
    vec = APPLY_2("json:map-ref", root, k);
    /* TRACE_S7_DUMP("m", m); */
    flag = APPLY_1("json:map?", vec);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("json:vector?", vec);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    res = APPLY_2("json:vector-ref", vec, s7_make_integer(s7, 1));
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // apply vector
    res = APPLY_OBJ(vec, s7_make_integer(s7, 0));
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(10, s7_integer(res));

    res = APPLY_OBJ(vec, s7_make_integer(s7, 1));
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(11, s7_integer(res));

    res = APPLY_OBJ(vec, s7_make_integer(s7, 2));
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(12, s7_integer(res));

    len = APPLY_1("json:vector-length", vec);
    flag = APPLY_1("integer?", len);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(3, s7_integer(len));
}

void float_vector_ops(void) {
    root = JSON_READ("{" S(v) ": [1.2, 3.14, 5.999] }");
    flag = APPLY_1("json:map?", root);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("json:vector?", root);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    k = s7_make_string(s7, "v");
    vec = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:map?", vec);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("json:vector?", vec);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    res = APPLY_2("json:vector-ref", vec, s7_make_integer(s7, 1));
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("real?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // apply vector
    res = APPLY_OBJ(vec, s7_make_integer(s7, 0));
    flag = APPLY_1("real?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_DOUBLE(1.2, s7_real(res));

    res = APPLY_OBJ(vec, s7_make_integer(s7, 1));
    flag = APPLY_1("real?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_DOUBLE(3.14, s7_real(res));

    res = APPLY_OBJ(vec, s7_make_integer(s7, 2));
    flag = APPLY_1("real?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_DOUBLE(5.999, s7_real(res));

    len = APPLY_1("json:vector-length", vec);
    flag = APPLY_1("integer?", len);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(3, s7_integer(len));
}

void string_vector_ops(void) {
    root = JSON_READ("{" S(v) ": [" S(a) ", "S(b) ", " S(c) "] }");
    flag = APPLY_1("json:map?", root);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("json:vector?", root);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    k = s7_make_string(s7, "v");
    vec = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:map?", vec);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("json:vector?", vec);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    res = APPLY_2("json:vector-ref", vec, s7_make_integer(s7, 0));
    TRACE_S7_DUMP("res", res);
    flag = APPLY_1("string?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("a", s7_string(res));

    // apply vector
    res = APPLY_OBJ(vec, s7_make_integer(s7, 0));
    flag = APPLY_1("string?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("a", s7_string(res));

    res = APPLY_OBJ(vec, s7_make_integer(s7, 1));
    flag = APPLY_1("string?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("b", s7_string(res));

    res = APPLY_OBJ(vec, s7_make_integer(s7, 2));
    flag = APPLY_1("string?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("c", s7_string(res));

    len = APPLY_1("json:vector-length", vec);
    flag = APPLY_1("integer?", len);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(3, s7_integer(len));
}

void bool_vector_ops(void) {
    root = JSON_READ("{" S(v) ": [true, false, true] }");
    flag = APPLY_1("json:map?", root);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("json:vector?", root);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    k = s7_make_string(s7, "v");
    vec = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:map?", vec);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("json:vector?", vec);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    res = APPLY_2("json:vector-ref", vec, s7_make_integer(s7, 1));
    flag = APPLY_1("boolean?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // apply vector
    res = APPLY_OBJ(vec, s7_make_integer(s7, 0));
    flag = APPLY_1("boolean?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(s7_t(s7), res);

    res = APPLY_OBJ(vec, s7_make_integer(s7, 1));
    TEST_ASSERT_EQUAL(s7_f(s7), res);

    res = APPLY_OBJ(vec, s7_make_integer(s7, 2));
    TEST_ASSERT_EQUAL(s7_t(s7), res);

    len = APPLY_1("json:vector-length", vec);
    flag = APPLY_1("integer?", len);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(3, s7_integer(len));
}

void mixed_vector_ops(void) {
    root = JSON_READ("{" S(v) ": [1, 3.14, true, " S(bye) "] }");
    flag = APPLY_1("json:map?", root);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("json:vector?", root);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    k = s7_make_string(s7, "v");
    vec = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:map?", vec);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("json:vector?", vec);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // apply vector
    res = APPLY_OBJ(vec, s7_make_integer(s7, 0));
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_INT(1, s7_integer(res));

    res = APPLY_OBJ(vec, s7_make_integer(s7, 1));
    flag = APPLY_1("number?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("real?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_DOUBLE(3.14, s7_real(res));

    res = APPLY_OBJ(vec, s7_make_integer(s7, 2));
    TEST_ASSERT_EQUAL(s7_t(s7), res);

    res = APPLY_OBJ(vec, s7_make_integer(s7, 3));
    TEST_ASSERT_EQUAL_STRING("bye", s7_string(res));

    len = APPLY_1("json:vector-length", vec);
    flag = APPLY_1("integer?", len);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL(4, s7_integer(len));
}

void vector_of_vectors(void) {
    root = JSON_READ("{" S(v) ": [[1, 2], [3, 4]] }");
    flag = APPLY_1("json:map?", root);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("json:vector?", root);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    k = s7_make_string(s7, "v");
    vec = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:map?", vec);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("json:vector?", vec);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // apply vector
    vec2 = APPLY_OBJ(vec, s7_make_integer(s7, 0));
    /* TRACE_S7_DUMP("res", res); */
    flag = APPLY_1("json:vector?", vec2);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    res = APPLY_OBJ(vec2, s7_make_integer(s7, 0));
    /* TRACE_S7_DUMP("res", res); */
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_INT(1, s7_integer(res));

    vec2 = APPLY_OBJ(vec, s7_make_integer(s7, 1));
    /* TRACE_S7_DUMP("res", res); */
    flag = APPLY_1("json:vector?", vec2);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    res = APPLY_OBJ(vec2, s7_make_integer(s7, 0));
    /* TRACE_S7_DUMP("res", res); */
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_INT(3, s7_integer(res));

}

void vector_of_maps(void) {
    root = JSON_READ("{" S(v) ": [{ " S(a) ": 0 }, {" S(b) ": " S(hi) " }]}");
    flag = APPLY_1("json:map?", root);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    flag = APPLY_1("json:vector?", root);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);

    k = s7_make_string(s7, "v");
    vec = APPLY_2("json:map-ref", root, k);
    flag = APPLY_1("json:map?", vec);
    TEST_ASSERT_EQUAL(s7_f(s7), flag);
    flag = APPLY_1("json:vector?", vec);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    // apply vector
    m = APPLY_OBJ(vec, s7_make_integer(s7, 0));
    /* TRACE_S7_DUMP("res", res); */
    flag = APPLY_1("json:map?", m);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    res = APPLY_OBJ(m, s7_make_string(s7, "a"));
    /* TRACE_S7_DUMP("res", res); */
    flag = APPLY_1("integer?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_INT(0, s7_integer(res));

    m = APPLY_OBJ(vec, s7_make_integer(s7, 1));
    flag = APPLY_1("json:map?", m);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);

    res = APPLY_OBJ(m, s7_make_string(s7, "b"));
    /* TRACE_S7_DUMP("res", res); */
    flag = APPLY_1("string?", res);
    TEST_ASSERT_EQUAL(s7_t(s7), flag);
    TEST_ASSERT_EQUAL_STRING("hi", s7_string(res));
}


/* void vector_refs(void) { */
/*     jo = JSON_READ("\"m = [1, 2, 3]\")"); */
/*     flag = APPLY_1("json:map?", jo); */
/*     TEST_ASSERT_EQUAL(s7_t(s7), flag); */

/*     k = s7_make_string(s7, "m"); */
/*     a = APPLY_2("json:map-ref", t, k); */
/*     flag = APPLY_1("json:array?", a); */
/*     TEST_ASSERT_EQUAL(s7_t(s7), flag); */

/*     /\* apply object to key: (t k) == (json:map-ref t k) *\/ */
/*     a = s7_apply_function(s7, t, s7_list(s7, 1, k)); */
/*     flag = APPLY_1("json:array?", a); */
/*     TEST_ASSERT_EQUAL(s7_t(s7), flag); */

/*     // try hash-object-ref - nope, segfault */
/*     /\* a = APPLY_2("hash-object-ref", t, k); *\/ */
/*     /\* flag = APPLY_1("json:array?", a); *\/ */
/*     /\* TEST_ASSERT_EQUAL(s7_t(s7), flag); *\/ */
/* } */

/* void vector_serialization(void) { */
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

/* void vector_to_string(void) { */
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

int main(int argc, char **argv)
{
    s7 = initialize("interpolation", argc, argv);

    libs7_load_clib(s7, "json");

    UNITY_BEGIN();

    RUN_TEST(int_vector_ops);
    RUN_TEST(float_vector_ops);
    RUN_TEST(string_vector_ops);
    RUN_TEST(bool_vector_ops);
    RUN_TEST(mixed_vector_ops);
    RUN_TEST(vector_of_vectors);
    RUN_TEST(vector_of_maps);

    return UNITY_END();
    s7_quit(s7);
    s7_free(s7);
}
