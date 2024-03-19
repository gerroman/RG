cmake_minimum_required(VERSION 3.9)
project(GiNaC)

add_custom_command(
  OUTPUT G-tm.cpp
  COMMAND "C:\\Wolfram\\Mathematica\\10.4\\SystemFiles\\Links\\WSTP\\DeveloperKit\\Windows-x86-64\\CompilerAdditions\\mldev64\\bin\\wsprep.exe" "${CMAKE_CURRENT_SOURCE_DIR}\\G.tm" -o G-tm.cpp
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/G.tm
  VERBATIM
)

FIND_PACKAGE(ginac)
FIND_PACKAGE(cln)

include_directories("C:\\Wolfram\\Mathematica\\10.4\\SystemFiles\\Links\\WSTP\\DeveloperKit\\Windows-x86-64\\CompilerAdditions\\mldev64\\include")
link_directories("C:\\Wolfram\\Mathematica\\10.4\\SystemFiles\\Links\\WSTP\\DeveloperKit\\Windows-x86-64\\CompilerAdditions\\mldev64\\lib")

set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(G G-tm.cpp G.cpp)
target_link_libraries(G wstp64i4 ginac cln)

install(TARGETS G)