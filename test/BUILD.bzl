load("//:BUILD.bzl",
     "GOPT_VERSION",
     "LIBUNITY_CC_VERSION",
     "UTHASH_VERSION")

## Args common to all test binarys:
TEST_SRCS = ["//test/unit:common.c", "//test/unit:common.h"]

TEST_DEPS = [
    "//src:s7", # always statically link libs7 for tests???
    "@gopt//:gopt",
    # "//vendored/gopt",
    "@unity//src:unity",
    # "//vendored/unity",
    "@uthash//src:uthash"
    # "//vendored/uthash",
]

TEST_COPTS = []

TEST_INCLUDE_PATHS = [
    "-Isrc", "-Iexternal/libs7/src",
    "-Itest/unit",
    "-Iexternal/gopt~{}/src".format(GOPT_VERSION),
    # "-Ivendored/gopt", "-Iexternal/libs7/vendored/gopt",
    "-Iexternal/unity~{}/src".format(LIBUNITY_CC_VERSION),
    # "-Ivendored/unity", "-Iexternal/libs7/vendored/unity",
    "-Iexternal/uthash~{}/src".format(UTHASH_VERSION)
    # "-Ivendored/uthash", "-Iexternal/libs7/vendored/uthash",
]

TEST_DEFINES = []

TEST_LINKOPTS = []

TIMEOUT = "short"


