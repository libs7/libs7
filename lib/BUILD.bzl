CLIB_COPTS = [
    "-Wall",
    "-Wextra",
    "-Wno-unused-parameter",

    "-Isrc",
    "-Iexternal/libs7/src",
] + select({
    "//:macos": [
        "-std=c11",
        "-Werror=pedantic",
        "-Wno-gnu",
        "-Wno-format-pedantic",
    ],
    "//:linux": [
        "-std=gnu11",
        "-fPIC",
        "-Wl,--no-undefined",
        # "--pedantic-errors",
    ],
    "//conditions:default": ["-std=c11"],
}) + select({
    "//config/clibs/link:shared?": ["-fPIC"],
    "//conditions:default": []
})

CLIB_LINKOPTS = select({
    "//config/host:macos": [],
    #FIXME: -rdynamic only on Linux + link:dynamic?
    "//config/host:linux": ["-rdynamic"],
    # non-linux: ["-Wl,-export-dynamic"],
    "//conditions:default": []
})

CLIB_DEFINES = [
    "WITH_SYSTEM_EXTRAS"
] + select({
    # "//config/s7:debug?": ["S7_DEBUGGING"],
    "//conditions:default":   []
})
