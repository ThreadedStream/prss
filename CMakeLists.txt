cmake_minimum_required(VERSION 3.12)

project(prss CXX)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/antlr/cmake)

#include(ExternalAntlr4Cpp)

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
add_subdirectory(scene_lang)


#message("Binaries reside in ${BUILD_DIR}")