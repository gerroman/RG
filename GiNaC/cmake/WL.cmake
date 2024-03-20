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
endif()

message("[note]: platform = '${WL_PLATFORM}'")

include_directories("${WL_INCLUDE_PATH}")
link_directories("${WL_LIB_PATH}")

function(wsprep fin fout)
	add_custom_command(
		OUTPUT "${fout}"
		COMMAND "${WL_BIN_PATH}/wsprep" "${fin}" -o "${fout}"
		DEPENDS "${fin}"
		VERBATIM
	)
endfunction()
