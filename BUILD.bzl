SRCS = ["//config:config.h", "//config:ansi_colors.h"]

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
] + select({
    "//config/host/build:macos?": [
        "-std=c11",
        "-Wno-gnu-statement-expression",
        # "-Werror=pedantic",
        # "-Wno-gnu",
        # "-Wno-format-pedantic",
    ],
    "//config/host/build:linux?": [
        "-std=gnu11",
        "-fPIC",
        # "-Wl,--no-undefined",
    ],
    "//conditions:default": ["-std=c11"],
})

DEPS = [ ## only vendored
    "//vendored/CException",
    "//vendored/logc",
]

INCLUDE_PATHS = [
    "-Iconfig", "-Iexternal/mustachios7/config",
    "-Ivendored/CException", "-Iexternal/mustachios/vendored/CException",
    "-Ivendored/logc", "-Iexternal/libs7/Ivendored/logc"
]

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
    "//lib/libmustache/syntax:alt?": ["ALT_SYNTAX"],
    "//conditions:default": []
}) + select({
    "//config/profile:dev?": ["DEVBUILD", "TRACING"],
    "//conditions:default": []
}) + select({
    "//config/debug:trace?": ["TRACING"],
    "//conditions:default": []
})
