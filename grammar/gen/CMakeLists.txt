set(ANTLR_DIR ${CMAKE_SOURCE_DIR}/antlr)
set(ANTLR_INCLUDE_DIR ${ANTLR_DIR}/runtime/src)
set(GEN_INCLUDE_DIR include)

find_library(ANTLR_SHARED_LIB NAMES libantlr4-runtime.so HINTS ${ANTLR_DIR}/dist)

file(GLOB_RECURSE HEADER_FILES include/*.h)
file(GLOB_RECURSE SRC_FILES src/*.cpp)

add_library(scene_parser SHARED ${HEADER_FILES} ${SRC_FILES})
target_include_directories(scene_parser PUBLIC ${ANTLR_INCLUDE_DIR} ${GEN_INCLUDE_DIR})
target_link_libraries(scene_parser PUBLIC ${ANTLR_SHARED_LIB})