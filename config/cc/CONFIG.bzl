load("@obazl_tools_cc//config:BASE.bzl",
     _BASE_COPTS    = "BASE_COPTS",
     "DSO_EXT",
     _BASE_LINKOPTS = "BASE_LINKOPTS",
     _define_module_version = "define_module_version")

define_module_version = _define_module_version

##########################################
# external use (assumes use of cc_config):
# LIBS7_SRCS = ["@libs7//lib:libs7.h"]
# LIBS7_DEPS = ["@libs7//lib:s7"]
# LIBS7_INCLUDE_PATHS = [
#     # "-I$(@libs7)/src",
#     # "-I$(GENDIR)/$(@libs7)/lib",
# ]

################
## internal use:
BASE_SRCS          = []
BASE_DEPS          = ["@liblogc//lib:logc"]
BASE_INCLUDE_PATHS = [
    # "-I$(@)/lib",
    # "-I$(GENDIR)/$(@)/lib"
]
BASE_COPTS         = _BASE_COPTS + BASE_INCLUDE_PATHS
BASE_LINKOPTS      = _BASE_LINKOPTS
BASE_DEFINES       = ["PROFILE_$(COMPILATION_MODE)"]
LOCAL_DEFINES      = DSO_EXT + select({
    "@platforms//os:linux": [
        "_GNU_SOURCE",          # for dlsym RTLD handles
    ],
    "//conditions:default"      : []
})
