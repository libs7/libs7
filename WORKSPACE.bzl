load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")

all_content = """
filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])
filegroup(name = "hdrs", srcs = glob(["include/**"]), visibility = ["//visibility:public"])
"""

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

    maybe(
        http_archive,
            name = "rules_foreign_cc",
        sha256 = "2a4d07cd64b0719b39a7c12218a3e507672b82a97b98c6a89d38565894cf7c51",
        strip_prefix = "rules_foreign_cc-0.9.0",
        url = "https://github.com/bazelbuild/rules_foreign_cc/archive/refs/tags/0.9.0.tar.gz",
        )

    # maybe(
    #     git_repository,
    #     name = "rules_cc",
    #     remote = "https://github.com/bazelbuild/rules_cc",
    #     commit = "b1c40e1de81913a3c40e5948f78719c28152486d",
    #     shallow_since = "1605101351 -0800"
    #     # branch = "master"
    # )

    # maybe(
    #     git_repository,
    #     name = "sealark",
    #     remote = "https://github.com/obazl/sealark",
    #     # commit = "b1c40e1de81913a3c40e5948f78719c28152486d",
    #     # shallow_since = "1605101351 -0800"
    #     branch = "main"
    # )

    # http://www.throwtheswitch.org/unity
    # maybe(
    #     http_archive,
    #     name = "unity",
    #     urls = [
    #         "https://github.com/ThrowTheSwitch/Unity/archive/refs/tags/v2.5.2.zip",
    #     ],
    #     strip_prefix = "v2.5.2",
    #     build_file_content = all_content,
    #     workspace_file_content = "workspace( name = \"unity\" )"
    # )

    ## for s7?
#     maybe(
#         http_archive,
#         name = "libffi",
#         url = "https://github.com/libffi/libffi/archive/refs/tags/v3.4.2.zip",
#         strip_prefix = "libffi-3.4.2",
#         build_file_content = all_content,
#         # sha256 = "72fba7922703ddfa7a028d513ac15a85c8d54c8d67f55fa5a4802885dc652056",
#         # build_file = "@//bzl/imports:libffi.BUILD",
#         ## the zip version requires use of autogen
#         #url = "https://github.com/libffi/libffi/archive/v3.3.zip",
#         # type = "zip",
#         # sha256 = "60b64c656520f986ec7bd2a6dc61e800848c97872f8f5132c5f753d9c205c358",
# )

#### s7 nrepl (notcurses repl)
# deps: libdeflate, libunistring, ncurses
################################################################
# build cmd:  bazel build @notcurses//:notcurses
## v 2.1.6 <=  v3.0
    maybe( ## build cmd:  bazel build @notcurses//:notcurses
        http_archive,
        name = "notcurses",
        build_file = "//imports/notcurses:BUILD.bazel",
        # build_file_content = all_content,

        ## 2.4.0 is first version with mac/win support, so this is our minimum:
        url = "https://github.com/dankamongmen/notcurses/archive/refs/tags/v2.4.0.zip",
        strip_prefix = "notcurses-2.4.0",

        # nope, won't build, missing uniwbrk.h from libunistring.h
        # url = "https://github.com/dankamongmen/notcurses/archive/refs/tags/v2.2.0.zip",
        # strip_prefix = "notcurses-2.2.0",
        # sha256 = "ac7906faae3bbf1fc88f38e8b25bd15ff11c89e4d6995bc82b4a458cf97b61c4"

        ## nope
        # url = "https://github.com/dankamongmen/notcurses/archive/refs/tags/v2.1.7.zip",
        # strip_prefix = "notcurses-2.1.7",

        ## nope, won't build:
        # url = "https://github.com/dankamongmen/notcurses/archive/refs/tags/v2.1.8.zip",
        # strip_prefix = "notcurses-2.1.8",
        # sha256 = "12802914da1d10feb8d264e267cf3b9173c5d6564a69229e7e31f819acbd9ac1"

        ## nope, incompatible:
        # url = "https://github.com/dankamongmen/notcurses/archive/refs/tags/v2.4.9.zip",
        # strip_prefix = "notcurses-2.4.9",
        # sha256 = "5cbab908153c96066061d2a2eef038bf31df9806e80eb1697c14905bd362fe06"

        ## nope, incompatible:
        # url = "https://github.com/dankamongmen/notcurses/archive/refs/tags/v3.0.9.zip",
        # strip_prefix = "notcurses-3.0.9",
        # sha256 = "ef891a5f07c9709c25ff7e16c776dd8f1e511106b4cd024d4475cf3c10c5f5e4"
    )
    # deps: libdeflate-dev libncurses-dev libunistring-dev

    ################################################################
    # cmd:  bazel build @libdeflate//:libdeflate
    maybe(
        http_archive,
        name = "libdeflate",
        url  = "https://github.com/ebiggers/libdeflate/archive/refs/tags/v1.11.tar.gz",
        sha256 = "c72f691293f41c6aee66d44ca2dcd24092161b312a1c4c3d591d5d25f26b1faf",
        strip_prefix = "libdeflate-1.11",
        build_file = "//imports/libdeflate:BUILD.bazel",
)

    maybe(
        http_archive,
        name = "libunistring",
        url  = "https://ftp.gnu.org/gnu/libunistring/libunistring-1.1.tar.gz",
        strip_prefix = "libunistring-1.1",
        sha256 = "a2252beeec830ac444b9f68d6b38ad883db19919db35b52222cf827c385bdb6a",
        build_file = "//imports/libunistring:BUILD.bazel",
    )

    maybe(
        http_archive,
        name = "ncurses",
        url  = "https://github.com/mirror/ncurses/archive/79b9071f2be20a24c7be031655a5638f6032f29f.zip",
        build_file = "//imports/ncurses:BUILD.bazel",
    )


