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
    # build cmd:  bazel build @mustachios7//:libmustachios7
    maybe(
        http_archive,
        name = "mustachios7",
        url = "https://gitlab.com/obazl/mustachios7/-/archive/cced836dde3dde5b2189894a1bfcdedafe587635/mustachios7-cced836dde3dde5b2189894a1bfcdedafe587635.tar.gz",
        strip_prefix = "mustachios7-cced836dde3dde5b2189894a1bfcdedafe587635"
        # sha256 = "4635bc12f65e0cbe3f6c5b1ed5b2376b5ec94a41"
    )

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
        url = "https://mirror2.sandyriver.net/pub/software/gnu/gsl/gsl-2.7.1.tar.gz",
        strip_prefix = "gsl-2.7.1",
        sha256 = "dcb0fbd43048832b757ff9942691a8dd70026d5da0ff85601e52687f6deeb34b"
    )

    # maybe(
    #     http_archive,
    #     name = "atlas",
    #     build_file = "@libs7//imports/libatlas:BUILD.bazel",
    #     url = "https://sourceforge.net/projects/math-atlas/files/Stable/3.10.3/atlas3.10.3.tar.bz2",
    # )

    ################################################################
    ## lib arb. deps: gmp, mpfr, flint
    # build cmd:  bazel build @arb//:libarb
    maybe(
        http_archive,
        name = "arb",
        build_file = "@libs7//imports/libarb:BUILD.bazel",
        url = "https://github.com/fredrik-johansson/arb/archive/refs/tags/2.23.0.zip",
        strip_prefix = "arb-2.23.0",
        sha256 = "330c26c89493f6d393738d85d899a45a524e874b4e1c6d0c3a922537127e82e7"
    )

    # build cmd:  bazel build @flint//:libflint
    maybe(
        http_archive,
        name = "flint",
        build_file = "@libs7//imports/libflint:BUILD.bazel",
        url = "https://github.com/flintlib/flint2/archive/refs/tags/v2.9.0.zip",
        strip_prefix = "flint2-2.9.0",
        sha256 = "468ef780155a8dac081ffd09f204873299deb133e6c4baa6b1c13724ebba2ef3"
    )

    # build cmd:  bazel build @gmp//:libgmp
    maybe(
        http_archive,
        name = "gmp",
        build_file = "@libs7//imports/libgmp:BUILD.bazel",
        url = "https://gmplib.org/download/gmp/gmp-6.2.1.tar.xz",
        strip_prefix = "gmp-6.2.1",
        sha256 = "fd4829912cddd12f84181c3451cc752be224643e87fac497b69edddadc49b4f2"
    )

    # build cmd:  bazel build @flint//:libflint
    maybe( # depends on gmp
        http_archive,
        name = "mpfr",
        build_file = "@libs7//imports/libmpfr:BUILD.bazel",
        url = "https://www.mpfr.org/mpfr-current/mpfr-4.2.0.tar.xz",
        strip_prefix = "mpfr-4.2.0",
        sha256 = "06a378df13501248c1b2db5aa977a2c8126ae849a9d9b7be2546fb4a9c26d993"
    )

    # build cmd:  bazel build @mpc//:libmpc
    maybe( # dependency of //lib/libarb:libarb_s7.c
        http_archive,
        name = "mpc",
        build_file = "@libs7//imports/libmpc:BUILD.bazel",
        url = "https://ftp.gnu.org/gnu/mpc/mpc-1.3.1.tar.gz",
        strip_prefix = "mpc-1.3.1",
        sha256 = "ab642492f5cf882b74aa0cb730cd410a81edcdbec895183ce930e706c1c759b8"
    )

