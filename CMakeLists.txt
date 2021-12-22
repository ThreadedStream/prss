cmake_minimum_required(VERSION 3.12)

project(prss CXX)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/antlr/cmake)

#include(ExternalAntlr4Cpp)
set(ANTLR_DIR ${CMAKE_SOURCE_DIR}/antlr)
set(ANTLR_DIR ${ANTLR_DIR} PARENT_SCOPE)

set(ANTLR_INCLUDE_DIR ${ANTLR_DIR}/runtime/src)
set(ANTLR_INCLUDE_DIR ${ANTLR_INCLUDE_DIR} PARENT_SCOPE)

set(ANTLR_EXECUTABLE ${CMAKE_SOURCE_DIR}/antlr-4.9.2-complete.jar)

#find_package(ANTLR REQUIRED)

find_package(PkgConfig)

if (NOT WIN32)
    find_package(X11)
    find_package(Threads REQUIRED)
endif()

set(BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(BUILD_DIR ${BUILD_DIR} PARENT_SCOPE)

#add_subdirectory(antlr)
add_subdirectory(grammar/gen)
add_subdirectory(prss)


#message("Binaries reside in ${BUILD_DIR}")