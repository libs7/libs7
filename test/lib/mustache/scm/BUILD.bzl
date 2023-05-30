SRCS = [
    "//test:common.c",
    "//test:common.h",
    "//test/lib/mustache:utils.c",
    "//test/lib/mustache:utils.h",
    # "//test/src:debug.h", # FIXME: select
    # "//test/src:c_stacktrace.h" # FIXME: select
]

LIBS7 = "//src:s7"
LIBS7_HDRS = ["-Isrc", "-Iexternal/libs7/src"]
LIBS7_ADAPTER = "//lib/libmustache:mustache_s7_archive"
LIBS7_ADAPTER_HDRS = ["-Ilib/libmustache",
                      "-Iexternal/libs7/lib/libmustache"]

COPTS_S7 = LIBS7_HDRS + LIBS7_ADAPTER_HDRS + ["-Idev"]
# COPTS_S7 = select({
#     "//adapter/scm:libs7?": [
#         LIBS7_HDRS,
#         LIBS7_ADAPTER_HDRS,
#         "-Ivendored"],
#     "//conditions:default" : [
#         LIBS7_HDRS,
#         LIBS7_ADAPTER_HDRS,
#         "-Ivendored"],
# }) + ["-Itest/unit/scm/s7"]

DEPS_S7 = [LIBS7_ADAPTER, "//dev"]
# select({
#     "//adapter/scm:libs7?":  [LIBS7_ADAPTER], # LIBS7],
#     "//conditions:default" : [LIBS7_ADAPTER] #, LIBS7]
# })
