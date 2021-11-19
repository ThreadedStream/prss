

set(CMAKE_CXX_STANDARD 17)

# I really want to replace this whole mess with INCLUDE_DIR variables
# produced by the call to find_package
set(GEN_DIR ${CMAKE_SOURCE_DIR}/grammar/gen)
set(ANTLR_DIR ${CMAKE_SOURCE_DIR}/antlr)
set(GEN_INCLUDE_DIR ${GEN_DIR}/include)
set(ANTLR_INCLUDE_DIR ${ANTLR_DIR}/runtime/src)
set(SCENE_LANG_INCLUDE_DIR include)

find_library(GEN_SHARED_LIB NAMES libprss_parser_antlr.so HINTS ${BUILD_DIR}/grammar/gen)
find_library(ANTLR_SHARED_LIB NAMES libantlr4-runtime.so HINTS ${ANTLR_DIR}/dist)

add_executable(prss_lang_antlr src/main.cpp include/PyParser.h)
target_include_directories(prss_lang_antlr PUBLIC ${GEN_INCLUDE_DIR} ${ANTLR_INCLUDE_DIR} ${SCENE_LANG_INCLUDE_DIR} ${CIMG_INCLUDE_DIR})
target_link_libraries(prss_lang_antlr PUBLIC ${GEN_SHARED_LIB} ${ANTLR_SHARED_LIB})
