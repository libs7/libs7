load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")  # buildifier: disable=load
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")  # buildifier: disable=load

all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""

def cc_fetch_repos():

    maybe(
        http_archive,
        name = "bazel_skylib",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.0.2/bazel-skylib-1.0.2.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.0.2/bazel-skylib-1.0.2.tar.gz",
        ],
        sha256 = "97e70364e9249702246c0e9444bccdc4b847bed1eb03c5a3ece4f83dfe6abc44",
    )

    maybe(
        git_repository,
        name = "rules_cc",
        remote = "https://github.com/bazelbuild/rules_cc",
        commit = "b1c40e1de81913a3c40e5948f78719c28152486d",
        shallow_since = "1605101351 -0800"
        # branch = "master"
    )

    maybe(
        http_archive,
            name = "rules_foreign_cc",
            sha256 = "6041f1374ff32ba711564374ad8e007aef77f71561a7ce784123b9b4b88614fc",
            strip_prefix = "rules_foreign_cc-0.8.0",
            url = "https://github.com/bazelbuild/rules_foreign_cc/archive/0.8.0.tar.gz",
        )

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

