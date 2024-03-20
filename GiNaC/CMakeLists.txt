cmake_minimum_required(VERSION 3.9)
project(GiNaC)

set(CMAKE_CXX_STANDARD 11)

set(WL_PLATFORM "Windows-x86-64")
set(WL_LIB "mldev64")
set(WL_BASE "$ENV{MATHEMATICA_BASE}")

message("environment variable MATHEMATICA_BASE = \'${WL_BASE}\'")

set(WL_COMPILER_PREFIX "${WL_BASE}/SystemFiles/Links/WSTP/DeveloperKit/${WL_PLATFORM}/CompilerAdditions/${WL_LIB}")
set(WL_LIB_PATH "${WL_BASE}/SystemFiles/Links/WSTP/DeveloperKit/${WL_PLATFORM}/SystemAdditions")

add_custom_command(
  OUTPUT G-tm.cpp
  COMMAND "${WL_COMPILER_PREFIX}/bin/wsprep.exe" "${CMAKE_CURRENT_SOURCE_DIR}/G.tm" -o G-tm.cpp
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/G.tm
  VERBATIM
)

FIND_PACKAGE(ginac)
FIND_PACKAGE(cln)

include_directories("${WL_COMPILER_PREFIX}/include")
link_directories("${WL_COMPILER_PREFIX}/lib")

set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(G G-tm.cpp G.cpp)
target_link_libraries(G wstp64i4 ginac cln)

install(TARGETS G)