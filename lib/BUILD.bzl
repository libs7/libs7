CLIB_COPTS = [
    "-Wall",
    "-Wextra",
    "-Werror=pedantic",
    "-Wno-format-pedantic",
    "-Wno-unused-parameter",

    "-Isrc",
    "-Iexternal/libs7/src",
] + select({
    "//:macos": [
        "-std=c11",
        "-Wno-gnu",
    ],
    "//:linux": [
        "-std=gnu11",
        "-fPIC",
        "-Wl,--no-undefined",
        # "--pedantic-errors",
    ],
    "//conditions:default": ["-std=c11"],
}) + select({
    "//config/clibs/link:dynamic?": ["-fPIC"],
    "//conditions:default": []
})

CLIB_LINKOPTS = select({
    "//config/host:macos": [],
    "//config/host:linux": ["-ldl", "-lm", "-Wl,-export-dynamic"],
    "//conditions:default": []
})

CLIB_DEFINES = [
    "WITH_SYSTEM_EXTRAS"
] + select({
    # "//config/s7:debug?": ["S7_DEBUGGING"],
    "//conditions:default":   []
})
