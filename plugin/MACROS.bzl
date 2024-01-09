load("@obazl_tools_cc//rules:module_profiles.bzl",
     "module_profiles")

BASE_COPTS = [
    "-x", "c",
    "-Wall",
    "-Wextra",
] + select({
    "@platforms//os:macos": [
        "-std=c11",
        "-Werror=pedantic", # not needed with -Werror?
        "-Wpedantic", # same as -pedantic, strict ISO C and ISO C++ warnings
        "-pedantic-errors",
        # "-Wno-gnu-statement-expression",
        # "-Werror=pedantic",
        # "-Wno-gnu",
        # "-Wno-format-pedantic",
    ],
    "@platforms//os:linux": [
        "-std=gnu11",
        "-fPIC",
        # GCC:
        "-Werror", # turn all warnings into errors
        "-Wfatal-errors", # stop on first error
        # "-Wl,--no-undefined",
    ],
    "//conditions:default": ["-std=c11"],
# }) + select({
#     "@platforms//cpu:arm64": [
#         # "-target", "arm64-apple-darwin22.5.0"
#     ],
#     "//conditions:default": [],
})

###################
def s7_plugin(name,
              alwayslink = True,
              linkstatic = True,
              linkopts = [],
              deps = [],
              copts = [],
              **kwargs):
    native.cc_library(
        name = name,
        alwayslink = alwayslink,
        linkstatic = linkstatic,
        deps = deps + [
        #deps = deps + [
            "@libs7//lib:s7",
            "@liblogc//lib:logc",
        ],
        linkopts = linkopts + select({
            "@platforms//os:linux": ["-Wl,--export-dynamic"],
            "@platforms//os:macos": ["-Wl,-export_dynamic"],
            "//conditions:default": []
        }),
        copts = BASE_COPTS + copts + [
            # "-I$(@libs7)/src",
            # "-I$(GENDIR)/$(@libs7)/src",
            # "-I$(@liblogc)/src",
        ],
        **kwargs
    )

####################
def s7_library(name,
              deps = [],
              copts = [],
              **kwargs):
    native.cc_library(
        name = name,
        deps = deps + [
            "@libs7//lib:s7",
            "@liblogc//lib:logc",
        ],
        copts = BASE_COPTS + copts + [
            # "-I$(@libs7)/src",
            # "-I$(GENDIR)/$(@libs7)/src",
            # "-I$(@liblogc)/src",
        ],
        **kwargs
    )

####################
def s7_plugin_test(name,
                   linkstatic = True,
                   deps = [],
                   copts = [],
                   **kwargs):
    native.cc_test(
        name = name,
        linkstatic    = linkstatic,
        deps = deps + [
            "@libs7//lib:s7",
            "@libs7//plugin:s7plugin_test_config",
            "@liblogc//lib:logc",
            "@gopt//lib:gopt",
            "@unity//lib:unity",
            "@uthash//lib:uthash",
        ],
        copts = BASE_COPTS + copts + [
            # "-I$(@libs7)/src",
            # "-I$(GENDIR)/$(@libs7)/src",
            # "-I$(GENDIR)/$(@libs7)/plugin",
            # "-I$(@liblogc)/src",
            # "-I$(@gopt)/src",
            # "-I$(@unity)/src",
            # "-I$(@uthash)/src",
        ],
        **kwargs
    )

#############################################
def s7_module_profiles(name, repos, visibility=None):
    if native.repository_name() == "@":
       _this = "."
    else:
        _this = "external/{}".format(
            native.repository_name()[1:])

    repos = repos + ["@liblogc//lib:logc",
                     "@libs7//lib:s7"]

    module_profiles(name = name, repos = repos,
                    this = _this,
                    visibility = ["//visibility:public"])
