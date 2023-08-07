load("//:BUILD.bzl",
     "GOPT_VERSION",
     "UNITY_VERSION",
     "UTHASH_VERSION")

## Args common to all test binarys:
TEST_SRCS = ["//test/unit:common.c", "//test/unit:common.h"]

TEST_DEPS = [
    "//src:s7",
    "@gopt//:gopt",
    "@unity//src:unity",
    "@uthash//src:uthash"
]

TEST_COPTS = []

TEST_INCLUDE_PATHS = [
    "-I$(GENDIR)/src",
    # "-Iexternal/libs7/src",
    "-Itest/unit",
    "-Iexternal/gopt~{}/src".format(GOPT_VERSION),
    "-Iexternal/unity~{}/src".format(UNITY_VERSION),
    "-Iexternal/uthash~{}/src".format(UTHASH_VERSION)
]

TEST_DEFINES = []

TEST_LINKOPTS = []

TIMEOUT = "short"


