load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")  # buildifier: disable=load
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")  # buildifier: disable=load

all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""

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
#         # build_file = "@//bzl/external:libffi.BUILD",
#         ## the zip version requires use of autogen
#         #url = "https://github.com/libffi/libffi/archive/v3.3.zip",
#         # type = "zip",
#         # sha256 = "60b64c656520f986ec7bd2a6dc61e800848c97872f8f5132c5f753d9c205c358",
# )

#### s7 nrepl (notcurses repl)

    maybe(
        http_archive,
        name = "notcurses",
        url  = "https://github.com/dankamongmen/notcurses/archive/refs/tags/v3.0.8.tar.gz",
        sha256 = "56c33ffe2a2bc4d0b6e3ac14bdf620cf41e3293789135f76825057d0166974fd",
        strip_prefix = "notcurses-3.0.8",
        build_file = "//external/notcurses:BUILD.bazel",
    )
    # deps: libdeflate-dev libncurses-dev libunistring-dev

    maybe(
        http_archive,
        name = "libdeflate",
        url  = "https://github.com/ebiggers/libdeflate/archive/refs/tags/v1.11.tar.gz",
        sha256 = "c72f691293f41c6aee66d44ca2dcd24092161b312a1c4c3d591d5d25f26b1faf",
        strip_prefix = "libdeflate-1.11",
        build_file = "//external/libdeflate:BUILD.bazel",
)

