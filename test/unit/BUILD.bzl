## Args common to all test binarys:
TEST_SRCS = ["//test/unit:common.c", "//test/unit:common.h"]

TEST_DEPS = [
    "//src:s7_archive", # always statically link libs7 for tests???
    "//vendored/gopt",
    "//vendored/unity",
    "//vendored/uthash",
]

TEST_COPTS = []

TEST_INCLUDE_PATHS = [
    "-Isrc", "-Iexternal/libs7/src",
    "-Itest/unit",
    "-Ivendored/gopt", "-Iexternal/libs7/vendored/gopt",
    "-Ivendored/unity", "-Iexternal/libs7/vendored/unity",
    "-Ivendored/uthash", "-Iexternal/libs7/vendored/uthash",
]

TEST_DEFINES = []

TEST_LINKOPTS = []

TIMEOUT = "short"


