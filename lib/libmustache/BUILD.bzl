MUSTACHE_INCLUDE_PATHS = [
    "-Ilib/libmustache/mustach",
    "-Iexternal/libs7/lib/libmustache/mustach",
    "-Ilib/libmustache/mustach/ds_mgr",
    "-Iexternal/libs7/lib/libmustache/mustach/ds_mgr"
]

MUSTACHE_DEPS = [
    "//lib/libmustache/mustach",
    "//lib/libmustache/mustach/ds_mgr:mustach_ds_mgr",
]
