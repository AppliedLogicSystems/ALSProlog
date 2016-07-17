cmake_minimum_required(VERSION 2.8)
project (als-prolog)

include(CTest)

set(CMAKE_MACOSX_RPATH 1)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32 -O0 -std=gnu99")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -m32")
#set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -m32")

if     (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	set(OS_FLAG UNIX_DARWIN)
	set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-no_pie")
elseif (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
	set(OS_FLAG UNIX_LINUX)
	set(OS_LIBS m dl crypt)
endif()

execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/../../version OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE VERSION)

include_directories(port unix generic smath)

add_definitions(-DUNIX -D${OS_FLAG} -DPORT -DVERSION=${VERSION}-cmake)

file(GLOB GENERIC_SOURCES "generic/*.c")
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/alspi_stub.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/ansi_engine.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/sioux_stub.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/winpimain.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/alspi_slib.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/dll_test.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/fsdos.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/fsmac.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/fsunix.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/fsvms.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/fswin32.c)
list(REMOVE_ITEM GENERIC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/generic/newpckgcoff.c)

file(GLOB PROC_SOURCES "port/*.c")

file(GLOB OS_SOURCES "unix/*.c")
list(REMOVE_ITEM OS_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/unix/unix_shared.c)
list(REMOVE_ITEM OS_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/unix/unix_static.c)

set(LIB_SOURCES ${GENERIC_SOURCES} ${PROC_SOURCES} ${OS_SOURCES})

set(STATIC_SOURCES unix/unix_static.c)
set(SHARED_SOURCES unix/unix_shared.c)

add_library(alspro-lib OBJECT ${LIB_SOURCES})

add_library(alspro-static STATIC $<TARGET_OBJECTS:alspro-lib> ${STATIC_SOURCES})
set_target_properties(alspro-static PROPERTIES OUTPUT_NAME alspro)

add_library(alspro-shared SHARED $<TARGET_OBJECTS:alspro-lib> ${SHARED_SOURCES})
set_target_properties(alspro-shared PROPERTIES OUTPUT_NAME alspro)

add_executable(alspro_b generic/topmain.c)
target_link_libraries(alspro_b alspro-static ${OS_LIBS})

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/alspro
  COMMAND alspro_b -q -b -obp ${CMAKE_CURRENT_SOURCE_DIR}/../als_dev/alsdev/ldr_alspro.pro -g "'save_image(alspro,[])'"
  DEPENDS alspro_b
)

add_custom_target(alspro-bin ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/alspro)

# Install

install (TARGETS alspro-static alspro-shared DESTINATION lib)
install_programs (/bin FILES alspro alspro.pst)
install (FILES generic/alspi.h generic/alspi_slib.h DESTINATION include)

# Tests

set(TESTS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tests)

add_test(HelloWorld alspro -q -b -g "write('hello world'), nl.")
set_tests_properties(HelloWorld PROPERTIES PASS_REGULAR_EXPRESSION "hello world")

add_test(command_line_test sh ${TESTS_DIR}/tsuite/test_command_line.sh ./alspro)
add_test(test_suit alspro -giac -b ${TESTS_DIR}/autotest.pro ${TESTS_DIR}/atest_db.pro -g run_tests -p -srcdir ${CMAKE_CURRENT_SOURCE_DIR})
add_test(iso_test_suite alspro -giac -b ${TESTS_DIR}/iso_test_suite/valid_so.pro -g "cd('${TESTS_DIR}/iso_test_suite'), run_all_tests.")

# Packaging

include (InstallRequiredSystemLibraries)
set (CPACK_GENERATOR "TGZ")
set (CPACK_RESOURCE_FILE_LICENSE  
     "${CMAKE_CURRENT_SOURCE_DIR}/../../LICENSE.txt")
set (CPACK_PACKAGE_VERSION "${VERSION}")
include (CPack)
