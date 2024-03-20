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
	set(CMAKE_EXECUTABLE_SUFFIX ".exe")
	find_package(UUID REQUIRED)
endif()

message("[note]: platform = '${WL_PLATFORM}'")

include_directories("${WL_INCLUDE_PATH}")
link_directories("${WL_LIB_PATH}")

macro(wsprep)
	add_custom_command(
		OUTPUT "${ARGV1}"
		COMMAND "${WL_BIN_PATH}/wsprep" "${ARGV0}" -o "${ARGV1}"
		DEPENDS "${ARGV0}"
		VERBATIM
	)
endmacro()

macro(wslink)
	if(${WL_PLATFORM} STREQUAL "Linux-x86-64")
		target_link_libraries(${ARGV} ${WL_LIB} uuid pthread rt dl)
	else()
		target_link_libraries(${ARGV} ${WL_LIB})
	endif()
endmacro()
