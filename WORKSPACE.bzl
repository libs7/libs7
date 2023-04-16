# load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")


#### notcurses deps: libdeflate, libunistring, ncurses
def fetch_repos():

    maybe(
        http_archive,
        name = "bazel_skylib",
        sha256 = "b8a1527901774180afc798aeb28c4634bdccf19c4d98e7bdd1ce79d1fe9aaad7",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.4.1/bazel-skylib-1.4.1.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.4.1/bazel-skylib-1.4.1.tar.gz",
        ],
    )

    ## needed to build some imported libs, e.g. notcurses, utf8proc, etc.
    maybe(
        http_archive,
        name = "rules_foreign_cc",
        sha256 = "2a4d07cd64b0719b39a7c12218a3e507672b82a97b98c6a89d38565894cf7c51",
        strip_prefix = "rules_foreign_cc-0.9.0",
        url = "https://github.com/bazelbuild/rules_foreign_cc/archive/refs/tags/0.9.0.tar.gz",
    )

    ################################################################
    ## supported exported libs ##

    # build cmd:  bazel build @utf8proc//:utf8proc
    maybe(
        http_archive,
        name = "utf8proc",
        build_file = "//imports/utf8proc:BUILD.bazel",
        url = "https://github.com/JuliaStrings/utf8proc/archive/1cb28a66ca79a0845e99433fd1056257456cef8b.zip",
        strip_prefix = "utf8proc-1cb28a66ca79a0845e99433fd1056257456cef8b",
        sha256 = "fd29e3dcb9d3bb0f50fd87638b63d8064e1bc6e2f2f66eea7d984ddc292f9500"
    )
