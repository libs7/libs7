COPTS = [
    "-x", "c",
    "-Wall",
    "-Wextra",
    # GCC:
    "-Werror", # turn all warnings into errors
    "-Werror=pedantic", # not needed with -Werror?
    "-Wpedantic", # same as -pedantic, strict ISO C and ISO C++ warnings
    "-pedantic-errors",
    "-Wfatal-errors", # stop on first error

    "-Isrc",
    "-Iexternal/libs7/src",
    "-Ilib", "-Iexternal/mustach/src",
    "-Ivendored/logc"
] + select({
    "//config/host/build:macos?": [
        "-std=c11",
        "-Wno-gnu-statement-expression"
    ],
    "//config/host/build:linux?": [
        "-std=gnu11",
        "-fPIC",
    ],
    "//conditions:default": ["-std=c11"],
})

LINKOPTS = select({
    "//config/host/build:linux?": ["-rdynamic"],
    "//conditions:default": []
})

DEFINES = select({
    "//config/host/build:macos?": ["DSO_EXT=\\\".dylib\\\""],
    "//config/host/build:linux?": [
        "DSO_EXT=\\\".so\\\"",
        "_XOPEN_SOURCE=500", # strdup
        # "_DEFAULT_SOURCE"    # dirent DT_* macros
    ],
    "//conditions:default":   ["DSO_EXT=\\\".so\\\""]
}) + select({
    "//lib/libmustachios7/syntax:alt?": ["ALT_SYNTAX"],
    "//conditions:default": []
}) + select({
    "//config/debug:debug?": ["DEBUGGING"],
    "//conditions:default": []
}) + select({
    "//config/debug:trace?": ["TRACING"],
    "//conditions:default": []
})


DEPS = [
    "@mustachios7//lib:mustach"
]
