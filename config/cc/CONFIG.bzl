load("@obazl_tools_cc//config:BASE.bzl",
     _BASE_COPTS    = "BASE_COPTS",
     "DSO_EXT",
     _define_module_version = "define_module_version")

define_module_version = _define_module_version

################
## internal use:
BASE_SRCS          = []
BASE_DEPS          = ["@liblogc//lib:logc"]
BASE_INCLUDE_PATHS = []
BASE_COPTS         = _BASE_COPTS + BASE_INCLUDE_PATHS
BASE_LINKOPTS      = []
BASE_DEFINES       = [
    "PROFILE_$(COMPILATION_MODE)",
]
LOCAL_DEFINES      = DSO_EXT + [
] + select({
    "@platforms//os:linux": [
        "_GNU_SOURCE",          # for dlsym RTLD handles
    ],
    "//conditions:default"      : []
})
