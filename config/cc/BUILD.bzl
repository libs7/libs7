COPTS = [
    "-x", "c",
    "-Wall",
    "-Wextra",
    "-Wno-unused-parameter",
] + select({
    "//config/host/build:macos?": [
        "-std=c11",
        "-Werror=pedantic",
        "-Wno-gnu",
        "-Wno-format-pedantic",
    ],
    "//config/host/build:linux?": [
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

