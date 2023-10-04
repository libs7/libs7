load("@cc_config//:CONFIG.bzl",
     _BASE_COPTS    = "BASE_COPTS",
     _BASE_LINKOPTS = "BASE_LINKOPTS")

##########################################
# external use (assumes use of cc_config):
LIBS7_SRCS = ["@libs7//src:libs7.h"]
LIBS7_DEPS = ["@libs7//src:s7"]
LIBS7_INCLUDE_PATHS = [
    "-I$(@libs7)/src",
    "-I$(GENDIR)/$(@libs7)/src",
]

################
## internal use:
BASE_SRCS          = []
BASE_DEPS          = ["@liblogc//src:logc"]
BASE_INCLUDE_PATHS = [
    "-I$(@liblogc)/src",
    "-I$(@)/src",
    "-I$(GENDIR)/$(@)/src"
]
BASE_COPTS         = _BASE_COPTS + BASE_INCLUDE_PATHS
BASE_LINKOPTS      = _BASE_LINKOPTS
BASE_DEFINES       = []
LOCAL_DEFINES      = ["DEBUG_$(COMPILATION_MODE)"] + select({
    "//config/host/build:linux?": [
        "_GNU_SOURCE",          # for dlsym RTLD handles
        "DSO_EXT=\\\".dylib\\\""
    ],
    "//config/host/build:macos?": ["DSO_EXT=\\\".dylib\\\""],
    # "//config/host/build:windows?": ["DSO_EXT=\\\".dll\\\""],
    "//conditions:default"      : ["DSO_EXT=\\\".so\\\""]
})
