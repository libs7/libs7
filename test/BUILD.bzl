## Args common to all test binarys:
DEFINES = select({
    "//config/clibs/link:runtime?": ["CLIBS_LINK_RUNTIME"],
    "//conditions:default":   []
}) + select({
    "//config/host:macos?": ["DSO_EXT=\\\".dylib\\\""],
    "//config/host:linux?": [
        "DSO_EXT=\\\".so\\\"",
        "_XOPEN_SOURCE=500", # strdup
        # "_DEFAULT_SOURCE"    # dirent DT_* macros
    ],
    "//conditions:default":   ["DSO_EXT=\\\".so\\\""]
}) + select({
    "//config/debug:debug?": ["DEBUG_TRACE"],
    "//conditions:default": []
}) + select({
    "//config/debug:trace?": ["TRACE"],
    "//conditions:default": []
})

DEPS = [
    ## "other libraries to be linked in to the binary target." can
    ## be cc_library targets but cc_binary targets with
    ## linkshared=1 won't work - put them in srcs (or data) attr.
    "//src:s7_archive", # always statically link libs7 for tests
    "//vendored/gopt",
    "//vendored/logc",
    "//vendored/unity",
    "//vendored/uthash",
]

COPTS = [
    "-x", "c",
    "-Wall",
    "-Wextra",
    "-Werror=pedantic",
    "-Wno-unused-parameter",
    "-Wno-format-pedantic",
    "-Isrc", "-Iexternal/libs7/src",
    "-Ivendored/gopt", "-Iexternal/libs7/vendored/gopt",
    "-Ivendored/logc", "-Iexternal/libs7/vendored/logc",
    "-Ivendored/unity", "-Iexternal/libs7/vendored/unity",
    "-Ivendored/uthash", "-Iexternal/libs7/vendored/uthash",
] + select({
    "//:macos": [
        "-Wno-gnu",
        "-std=c11"
    ],
    "//:linux": [
        "-std=gnu11",
        "--pedantic-errors",
    ],
    "//conditions:default": ["-std=c11"],
})

LINKOPTS = select({
    "//:linux": ["-rdynamic"],
    "//conditions:default": []
})

TIMEOUT = "short"


