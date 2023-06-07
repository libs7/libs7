//FIXME: rename to MST_RENDER_OBJ_TEST & share with json, scm
#define S7_RENDER_OBJ_TEST(sink, t, datum, expected)                    \
    do {                                                                \
        s7_pointer tplt = s7_make_string(s7, t);                        \
        s7_pointer actual = s7_call(s7, mustache_render,                \
                                    s7_list(s7, 3, sink, tplt, datum)); \
        if (actual == 0) {                                              \
            TEST_FAIL_MESSAGE("mustache:render error");                 \
        } else {                                                        \
            TEST_ASSERT_EQUAL_STRING(expected, s7_string(actual));      \
        }                                                               \
    } while(0)

#define TOML_RENDER_TEST(t, d, expected)                               \
    do {                                                                \
        s7_pointer tt = s7_apply_function(s7, toml_read,                \
                                          s7_list(s7, 1,                \
                                                  s7_make_string(s7, d))); \
        if (tt == 0) {                                                  \
            TEST_FAIL_MESSAGE("toml_read error");                       \
        } else {                                                        \
            s7_pointer tplt = s7_make_string(s7, t);                    \
            s7_pointer actual = s7_call(s7, mustache_render,            \
                                        s7_list(s7, 3, s7_f(s7), tplt, tt)); \
            if (actual == 0) {                                          \
                TEST_FAIL_MESSAGE("mustache:render error");             \
            } else {                                                    \
                TEST_ASSERT_EQUAL_STRING(expected, s7_string(actual)); \
            }                                                           \
        }                                                               \
    } while(0)

#define TOML_RENDER_SINK_TEST(sink, t, d, expected)                      \
    do {                                                                \
        s7_pointer tt = s7_apply_function(s7, toml_read,                \
                                          s7_list(s7, 1,                \
                                                  s7_make_string(s7, d))); \
        if (tt == 0) {                                                  \
            TEST_FAIL_MESSAGE("toml_read error");                       \
        } else {                                                        \
            s7_pointer tplt = s7_make_string(s7, t);                    \
            s7_pointer actual = s7_call(s7, mustache_render,            \
                                        s7_list(s7, 3, sink, tplt, tt)); \
            if (actual == 0) {                                          \
                TEST_FAIL_MESSAGE("mustache:render error");             \
            } else {                                                    \
                TEST_ASSERT_EQUAL_STRING(expected, s7_string(actual)); \
            }                                                           \
        }                                                               \
    } while(0)
