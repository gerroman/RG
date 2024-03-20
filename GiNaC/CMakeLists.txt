cmake_minimum_required(VERSION 3.9)
project(GiNaC)

set(CMAKE_CXX_STANDARD 11)

set(WL_BASE "$ENV{MATHEMATICA_BASE}")
message("[note]: environment variable MATHEMATICA_BASE = '${WL_BASE}'")

if(${CMAKE_SYSTEM_NAME} STREQUAL "Windows") 
	set(WL_PLATFORM "Windows-x86-64")
	set(WL_COMPILER_PREFIX "${WL_BASE}/SystemFiles/Links/WSTP/DeveloperKit/${WL_PLATFORM}/CompilerAdditions/mldev64")
	set(WL_BIN_PATH "${WL_COMPILER_PREFIX}/bin")
	set(WL_LIB_PATH "${WL_COMPILER_PREFIX}/lib")
  set(WL_INCLUDE_PATH "${WL_COMPILER_PREFIX}/include")
  set(WL_LIB "wstp64i4")
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(WL_PLATFORM "Linux-x86-64")
	set(WL_COMPILER_PREFIX "${WL_BASE}/SystemFiles/Links/WSTP/DeveloperKit/${WL_PLATFORM}/CompilerAdditions")
	set(WL_BIN_PATH "${WL_COMPILER_PREFIX}")
	set(WL_LIB_PATH "${WL_COMPILER_PREFIX}")
  set(WL_INCLUDE_PATH "${WL_COMPILER_PREFIX}")
  set(WL_LIB "WSTP64i4")
	set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
  set(CMAKE_EXECUTABLE_SUFFIX ".exe")
endif()

message("[note]: platform = '${WL_PLATFORM}'")

add_custom_command(
  OUTPUT G-tm.cpp
  COMMAND "${WL_BIN_PATH}/wsprep" "${CMAKE_CURRENT_SOURCE_DIR}/G.tm" -o G-tm.cpp
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/G.tm
  VERBATIM
)

FIND_PACKAGE(GiNaC REQUIRED)
FIND_PACKAGE(CLN REQUIRED)
FIND_PACKAGE(UUID REQUIRED)

include_directories("${WL_INCLUDE_PATH}")
link_directories("${WL_LIB_PATH}")

set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(G G-tm.cpp G.cpp)
target_link_libraries(G ${WL_LIB} ginac cln pthread uuid rt)

install(TARGETS G)
