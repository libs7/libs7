S7_LOGGING_MACROS = select({
    "@libs7//logging:fastbuild?": [
        "@libs7//logging:ansi_colors.h",
        "@libs7//logging:s7_macros_debug.h"
        ],
    "//conditions:default": [
        "@libs7//logging:s7_macros_ndebug.h"
        ]
})
