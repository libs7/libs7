###############################
def _makeheaders_impl(ctx):

    print("makeheaders: {m}".format(m=ctx.label))

    outs = []

    for src in ctx.files.srcs:
        print("src: {s}, ext: {x}".format(
            s=src, x=src.extension))
        outs.append(src.basename)

    for out in outs:
        print("out: %s" % out.removesuffix("c") + "h")

    defaultInfo = DefaultInfo(
    )

    return [defaultInfo]

####################
makeheaders = rule(
    implementation = _makeheaders_impl,
    doc = "Runs makeheaders utility.",
    attrs = dict(
        srcs = attr.label_list(
            doc = "src files for which headers should be emitted",
            allow_files = [".c"], # ".cxx"?
        ),
        xsrcs = attr.label_list(
            doc = ".c src files to be used as inputs but for which headers should not be emitted",
            allow_files = True,
        ),
        hdrs = attr.label_list(
            doc = "static header files to be used as input",
            allow_files = [".h"]
        ),
        opts = attr.string_list(
            # -h, -H
        ),
        _tool = attr.label(
            doc = "Bazel label of makeheaders executable.",
            mandatory = False,
            allow_single_file = True,
            executable = True,
            cfg = "exec",
            default = ":makeheaders"
        ),
    ),

    # fragments = ["platform", "cpp"],
    # host_fragments = ["platform",  "cpp"],
    toolchains = ["@bazel_tools//tools/cpp:toolchain_type"]
)

