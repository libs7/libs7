LIBS7_VERSION = "0.1.0"

LIBLOG_CC_VERSION = "1.0.0"
GOPT_VERSION = "10.0.0"
CEXCEPTION_VERSION = "1.3.3"
UNITY_VERSION = "2.5.2"
UTHASH_VERSION = "2.3.0"

CJSON_S7_VERSION = "1.7.16"
CJSON_VERSION = "1.7.16"
CWALK_VERSION = "1.2.7"
TOML_S7_VERSION = "0.1.0"
LIBTOML_CC_VERSION = "0.1.0"
MUSTACHIOS_VERSION = "0.1.0"

GDBM_VERSION = "1.23.0"
LIBUTF8PROC_CC_VERSION = "2.8.0"

BASE_SRCS = []

BASE_COPTS = [
    "-x", "c",
    # "-mcpu=arm64",
    "-target", "arm64-apple-darwin22.5.0",
    "-Wall",
    "-Wextra",
    # GCC:
    # "-Werror", # turn all warnings into errors
    # "-Wfatal-errors", # stop on first error
] + select({
    "@platforms//os:macos": [
        "-std=c11",
        "-Werror=pedantic", # not needed with -Werror?
        "-Wpedantic", # same as -pedantic, strict ISO C and ISO C++ warnings
        "-pedantic-errors",
        "-Wno-gnu-statement-expression",
        # "-Werror=pedantic",
        # "-Wno-gnu",
        # "-Wno-format-pedantic",
    ],
    "@platforms//os:linux": [
        "-std=gnu11",
        "-fPIC",
        # "-Wl,--no-undefined",
    ],
    "//conditions:default": ["-std=c11"],
})

BASE_DEPS = [
    "//config:hdrs",
    # "@cexception//lib:CException",
    "@liblog_cc//src:logc",
]

BASE_INCLUDE_PATHS = [
    "-Iconfig",
    "-Iexternal/libs7~{}/config".format(LIBS7_VERSION),

    "-Isrc",
    "-Iexternal/libs7~{}/src".format(LIBS7_VERSION),

    # "-Iexternal/cexception~{}/lib".format(CEXCEPTION_VERSION),

    "-Iexternal/liblog_cc~{}/src".format(LIBLOG_CC_VERSION),
]

BASE_LINKOPTS = select({
    "@platforms//os:linux": ["-rdynamic", "-ldl"],
    "@platforms//os:macos": [], ## "-ldl"],
    "//conditions:default": []
})

BASE_DEFINES = select({
    "@platforms//os:macos": ["DSO_EXT=\\\".dylib\\\""],
    "@platforms//os:linux": [
        "DSO_EXT=\\\".so\\\"",
        ## "_XOPEN_SOURCE=500", # strdup
        "_POSIX_C_SOURCE=200809L", # strdup, strndup since glibc 2.10
        "_DEFAULT_SOURCE",    # dirent macros
        "_GNU_SOURCE"         # dlsym RTLD macros
    ],
    "//conditions:default":   ["DSO_EXT=\\\".so\\\""]
}) + select({
        "@libs7//config/s7:debug?": ["S7_DEVBUILD"],
        "//conditions:default":   []
# }) + select({
#     "@libs7//lib/libmustachios/syntax:alt?": ["ALT_SYNTAX"],
#     "//conditions:default": []
}) + select({
    "@libs7//config/profile:dev?": ["DEVBUILD", "TRACING"],
    "//conditions:default": []
}) + select({
    "@libs7//config/debug:trace?": ["TRACING"],
    "//conditions:default": []
})
