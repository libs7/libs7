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
    # build cmd:  bazel build @libunistring//:libunistring
    maybe( ## WARNING: takes a loong time to build
        http_archive,
        name = "libunistring",
        build_file = "@libs7//imports/libunistring:BUILD.bazel",
        url = "https://ftp.gnu.org/gnu/libunistring/libunistring-1.1.tar.gz",
        strip_prefix = "libunistring-1.1",
        sha256 = "a2252beeec830ac444b9f68d6b38ad883db19919db35b52222cf827c385bdb6a"
    )

    # build cmd:  bazel build @utf8proc//:libutf8proc
    maybe(
        http_archive,
        name = "utf8proc",
        build_file = "@libs7//imports/libutf8proc:BUILD.bazel",
        url = "https://github.com/JuliaStrings/utf8proc/archive/refs/tags/v2.8.0.zip",
        strip_prefix = "utf8proc-2.8.0",
        sha256 = "b2aec990876d1a995baf96150bb351e9724a29a936aa7b24078c531228236d3a"
    )

    # build cmd:  bazel build @gdbm//:libgdbm
    maybe(
        http_archive,
        name = "gdbm",
        build_file = "@libs7//imports/libgdbm:BUILD.bazel",
        url = "https://ftp.gnu.org/gnu/gdbm/gdbm-1.23.tar.gz",
        strip_prefix = "gdbm-1.23",
        sha256 = "74b1081d21fff13ae4bd7c16e5d6e504a4c26f7cde1dca0d963a484174bbcacd"
    )

    # build cmd:  bazel build @gsl//:libgsl
    maybe(
        http_archive,
        name = "gsl",
        build_file = "@libs7//imports/libgsl:BUILD.bazel",
        # url = "https://mirrors.ocf.berkeley.edu/gnu/gsl/gsl-2.2.tar.gz",
        # strip_prefix = "gsl-2.2",
        # sha256 = "4de40a9a79ea42a127928b095d066993cb845812d657434e29e987240113d8df"

        url = "https://mirror2.sandyriver.net/pub/software/gnu/gsl/gsl-2.7.1.tar.gz",
        strip_prefix = "gsl-2.7.1",
        sha256 = "dcb0fbd43048832b757ff9942691a8dd70026d5da0ff85601e52687f6deeb34b"
    )

