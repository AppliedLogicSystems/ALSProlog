cmake_minimum_required(VERSION 3.6)

set(CMAKE_MACOSX_RPATH 1)

add_compile_options(-m32 -O0 -std=gnu99)
set(CMAKE_EXE_LINKER_FLAGS "-m32 -Wl,-no_pie")

execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/../../version OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE VERSION)

include_directories(port unix generic smath)

add_definitions(-DUNIX -DUNIX_DARWIN -DPORT -DVERSION=${VERSION}-cmake)

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
target_link_libraries(alspro_b alspro-static)
