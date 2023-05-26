COPTS_TEST = [
    "-Itest",
    "-Iexternal/libs7/test",
    "-Itest/lib/mustache",
    "-Iexternal/libs7/test/lib/mustache", # common.[ch]
    # "-Itest/src",
    # "-Iexternal/libs7/test/src", # ansi_colors.h
    "-Ivendored/gopt", "-Iexternal/libs7/vendored/gopt",
    "-Ivendored/logc", "-Iexternal/libs7/vendored/logc",
    "-Ivendored/unity", "-Iexternal/libs7/vendored/unity",
    "-Ivendored/uthash", "-Iexternal/libs7/vendored/uthash",
]

DEFINES_TEST = select({
    "//config/debug:stacktrace?": ["STACKTRACE"],
    "//conditions:default" : []
})

DEPS_TEST = [
    "//vendored/gopt",
    "//vendored/logc",
    "//vendored/unity",
    "//vendored/uthash",
]

TIMEOUT = "short"
