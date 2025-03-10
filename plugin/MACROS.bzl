load("@rules_cc//cc:defs.bzl", "cc_library", "cc_shared_library", "cc_test")
load("@obazl_tools_cc//config:BASE.bzl", "BASE_COPTS")

###################
def s7_plugin(name,
              alwayslink = True,
              linkstatic = True,
              linkopts = [],
              deps = [],
              copts = [],
              **kwargs):

    cc_library(
        name = name,
        alwayslink = alwayslink,
        linkstatic = linkstatic,
        copts = BASE_COPTS + copts,
        deps = deps + [
            "@libs7//lib:s7",
            "@liblogc//lib:logc",
        ],
        linkopts = linkopts + select({
            "@platforms//os:linux": ["-Wl,--export-dynamic"],
            "@platforms//os:macos": ["-Wl,-export_dynamic"],
            "//conditions:default": []
        }),
        **kwargs
    )

####################
def _s7_plugin_test_impl(name,
                        libunity = False,
                        linkstatic = True,
                        deps = [],
                        copts = [],
                        visibility = ["//visibility:public"],
                        **kwargs):
    if libunity:
        udep = [libunity]
        uhdr = [
            "-Iexternal/{}/src".format(Label("@unity").repo_name)
        ]
    else:
        udep = [Label("@unity//lib:unity")]
        uhdr = []

    cc_test(
        name       = name,
        linkstatic = linkstatic,
        copts      = BASE_COPTS + copts + uhdr,
        deps       = deps + [
            "@libs7//lib:s7",
            "@libs7//plugin:s7plugin_test_config",
            "@liblogc//lib:logc",
            "@gopt//lib:gopt",
            "@uthash//lib:uthash",
        ] + udep,
        visibility = visibility,
        **kwargs
    )

s7_plugin_test = macro(
    inherit_attrs = native.cc_test,
    implementation = _s7_plugin_test_impl,
    attrs = {
        "libunity": attr.label(
            configurable = False),
        "linkstatic": attr.bool(
            configurable = False,
            default = True),
    }
)

####################
def s7_library(name,
              deps = [],
              copts = [],
              **kwargs):
    cc_library(
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
def s7_shared_library(name,
                      deps = [],
                      copts = [],
                      **kwargs):
    cc_shared_library(
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

