load("@bazel_skylib//rules:common_settings.bzl", "BuildSettingInfo")

## generic mustachios runner.

DISABLED_FEATURES = [
    "module_maps",
]

########################
def _test_mustachios_impl(ctx):

    # args: script, data, template, out
    # only script and out are passed to the executable
    # the others just need to be in the runfiles (data),
    # the script is responsible for loading them.

    args = ctx.actions.args()
    if not ctx.file.data.extension in ["json", "toml", "scm", "dune"]:
        if ctx.file.data.basename != "dune":
            fail("Data arg extension must be json, toml, or scm; or name must be 'dune': %s" % ctx.file.data)

    args.add_all(["-s", ctx.file.script.path])
    args.add_all(["-o", ctx.outputs.out.path])
    args.add_all(["-t", ctx.file.template.path])
    args.add_all(["-d", ctx.file.data.path])

    # for var in ctx.var:
    #     print("VAR: {k}: {v}".format(k=var, v=ctx.var[var]))

    # runfiles = ctx.runfiles(files = ctx.files.data)
    # transitive_runfiles = []
    # for attr in (
    #     ctx.attr.script,
    #     ctx.attr.data,
    #     ctx.attr.template,
    #     # ctx.attr._tool,
    # ):
    #     # for t in attr:
    #     transitive_runfiles.append(attr[DefaultInfo].default_runfiles)
    # runfiles = runfiles.merge_all(transitive_runfiles)

    ctx.actions.run(
        mnemonic = "Mustachios",
        executable = ctx.file._tool,
        arguments = [args],
        inputs = depset(
            [ctx.file.script, ctx.file.template, ctx.file.data
             ] + ctx.attr._tool[DefaultInfo].default_runfiles.files.to_list()
        ),
        outputs = [ctx.outputs.out],
    )

    ########
    return [
        # DefaultInfo(files = depset([outfile]))
        DefaultInfo(files = depset([ctx.outputs.out]))
    ]

####################
test_mustachios = rule(
    implementation = _test_mustachios_impl,
    attrs = {
        "_tool": attr.label(
            # doc = "",
            executable = True,
            allow_single_file = True,
            # allow_files = True,
            # mandatory = True,
            default = ":mustachios",
            cfg = "exec",
        ),
        "out": attr.output(mandatory = True),
        "script": attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
        "data": attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
        "template": attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
    },
)
