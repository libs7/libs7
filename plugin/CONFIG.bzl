PLUGIN_TEST_DEPS  = [
    "@gopt//lib:gopt",
    "@libs7//plugin:s7plugin_test_config",
    "@unity//lib:unity",
    "@uthash//lib:uthash",
    # @liblogc already a dep of s7_plugin, etc.
]

PLUGIN_TEST_INCLUDE_PATHS = [
    # "-I$(@gopt)/src",
    # "-I$(@liblogc)/src",
    # "-I$(@libs7)/src",
    # "-I$(GENDIR)/$(@libs7)/src",
    # "-I$(GENDIR)/$(@libs7)/plugin",
    # "-I$(@unity)/src",
    # "-I$(@uthash)/src",
]

