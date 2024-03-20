find_package(PkgConfig)
pkg_check_modules(CLN REQUIRED cln)
include_directories(${CLN_INCLUDE_DIRS})
link_directories(${CLN_LIBRARY_DIRS})
