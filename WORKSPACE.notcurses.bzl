load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")

#### notcurses deps: libdeflate, libunistring, ncurses
def fetch_repos():

    # build cmd:  bazel build @notcurses//:notcurses
    maybe(
        http_archive,
        name = "notcurses",
        build_file = "//imports/libnotcurses:BUILD.bazel",
        url = "https://github.com/dankamongmen/notcurses/archive/refs/tags/v3.0.9.zip",
        strip_prefix = "notcurses-3.0.9",
        sha256 = "ef891a5f07c9709c25ff7e16c776dd8f1e511106b4cd024d4475cf3c10c5f5e4"
    )

    ################################################################
    # build cmd:  bazel build @libdeflate//:libdeflate
    maybe(
        http_archive,
        name = "libdeflate",
        url  = "https://github.com/ebiggers/libdeflate/archive/refs/tags/v1.11.tar.gz",
        sha256 = "c72f691293f41c6aee66d44ca2dcd24092161b312a1c4c3d591d5d25f26b1faf",
        strip_prefix = "libdeflate-1.11",
        build_file = "//imports/libdeflate:BUILD.bazel",
    )

    # build cmd: bazel build @libunistring//:libunistring
    maybe(
        http_archive,
        name = "libunistring",
        url  = "https://ftp.gnu.org/gnu/libunistring/libunistring-1.1.tar.gz",
        strip_prefix = "libunistring-1.1",
        sha256 = "a2252beeec830ac444b9f68d6b38ad883db19919db35b52222cf827c385bdb6a",
        build_file = "//imports/libunistring:BUILD.bazel",
    )

    ################
    maybe(
        http_archive,
        name = "ncurses",
        url  = "https://github.com/mirror/ncurses/archive/79b9071f2be20a24c7be031655a5638f6032f29f.zip",
        strip_prefix = "ncurses-79b9071f2be20a24c7be031655a5638f6032f29f",
        build_file = "//imports/libncurses:BUILD.bazel",
    )


