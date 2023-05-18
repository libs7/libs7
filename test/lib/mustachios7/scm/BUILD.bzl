SRCS = [
    "//test:common.c",
    "//test:common.h",
    "//test/lib/mustachios7:ansi_colors.h",
    "//test/lib/mustachios7:trace.h",
    "//test/lib/mustachios7:utils.c",
    "//test/lib/mustachios7:utils.h",
    # "//test/src:debug.h", # FIXME: select
    # "//test/src:c_stacktrace.h" # FIXME: select
]

LIBS7 = "//src:s7"
LIBS7_HDRS = ["-Isrc", "-Iexternal/libs7/src"]
LIBS7_ADAPTER = "//lib/libmustachios7:mustachios7_s7_archive"
LIBS7_ADAPTER_HDRS = ["-Ilib/libmustachios7",
                      "-Iexternal/libs7/lib/libmustachios7"]

COPTS_S7 = LIBS7_HDRS + LIBS7_ADAPTER_HDRS
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

DEPS_S7 = [LIBS7_ADAPTER]
# select({
#     "//adapter/scm:libs7?":  [LIBS7_ADAPTER], # LIBS7],
#     "//conditions:default" : [LIBS7_ADAPTER] #, LIBS7]
# })
