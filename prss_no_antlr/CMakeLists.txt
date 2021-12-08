set(CMAKE_CXX_STANDARD 17)

find_library(ANTLR_SHARED_LIB NAMES libantlr4-runtime.so HINTS ${ANTLR_DIR}/dist)
find_library(GEN_SHARED_LIB NAMES libprss_parser_antlr.so HINTS ${BUILD_DIR}/grammar/gen)

set(GEN_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/grammar/gen/include)
set(COMPILER_FLAGS "-Wall")

add_executable(prss_no_antlr main.cpp parser.hpp parser.cpp)
target_compile_options(prss_no_antlr PUBLIC "-g" ${COMPILER_FLAGS})
target_include_directories(prss_no_antlr PUBLIC ${ANTLR_INCLUDE_DIR} ${GEN_INCLUDE_DIR})
target_link_libraries(prss_no_antlr PUBLIC  ${ANTLR_SHARED_LIB} ${GEN_SHARED_LIB})
