COPTS = [
    "-Wall",
    "-Wextra",
    # "-Werror=pedantic",
    "-Wno-unused-parameter",
    "-Isrc",
] + select({
    "//:macos": [
        "-std=c11",
        "-Wno-format-pedantic",
        "-Wno-gnu"
    ],
    "//:linux": [
        "-std=gnu11",
        "-Wno-unused-function",
        "-Wno-implicit-fallthrough",
        # "--pedantic-errors",
        ],
    "//conditions:default": ["-std=c11"],
})
