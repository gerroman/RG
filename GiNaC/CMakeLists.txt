cmake_minimum_required(VERSION 3.9)

project(GiNaC)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -Wextra")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wall -Wextra")
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

include(WL)
find_package(GiNaC REQUIRED)
find_package(CLN REQUIRED)

wsprep(${CMAKE_CURRENT_SOURCE_DIR}/G.tm G-tm.cpp)

add_executable(G G-tm.cpp G.cpp)
if(${WL_PLATFORM} STREQUAL "Linux-x86-64")
  target_link_libraries(G ${WL_LIB} uuid pthread rt dl ginac cln)
else()
  target_link_libraries(G ${WL_LIB} ginac cln)
endif()


install(TARGETS G DESTINATION "${CMAKE_CURRENT_SOURCE_DIR}/package/bin")
