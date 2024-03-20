find_package(PkgConfig)
pkg_check_modules(GINAC REQUIRED ginac)
include_directories(${GINAC_INCLUDE_DIRS})
link_directories(${GINAC_LIBRARY_DIRS})
