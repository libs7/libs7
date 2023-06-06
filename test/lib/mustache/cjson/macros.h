
#define S7_RENDER_TEST(t, d, expected)                                \
    do {                                                                \
        s7_pointer jm = s7_apply_function(s7, json_read,                \
                                          s7_list(s7, 1,                \
                                                  s7_make_string(s7, d))); \
        if (jm == 0) {                                                  \
            TEST_FAIL_MESSAGE("json_read error");                       \
        } else {                                                        \
            s7_pointer tplt = s7_make_string(s7, t);                    \
            s7_pointer actual = s7_call(s7, mustache_render,            \
                                        s7_list(s7, 2, tplt, jm));      \
            if (actual == 0) {                                          \
                TEST_FAIL_MESSAGE("mustache:render error");             \
            } else {                                                    \
                TEST_ASSERT_EQUAL_STRING(expected, s7_string(actual)); \
            }                                                           \
        }                                                               \
    } while(0)
