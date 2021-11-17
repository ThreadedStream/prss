

# I really want to replace this whole mess with INCLUDE_DIR variables
# produces by the call to find_package
set(GEN_DIR ${CMAKE_SOURCE_DIR}/grammar/gen)
set(ANTLR_DIR ${CMAKE_SOURCE_DIR}/antlr)
set(GEN_INCLUDE_DIR ${GEN_DIR}/include)
set(ANTLR_INCLUDE_DIR ${ANTLR_DIR}/runtime/src)
set(SCENE_LANG_INCLUDE_DIR include)
set(CIMG_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/thirdparty)

file(GLOB_RECURSE HEADER_FILES include/*.h)
file(GLOB_RECURSE SRC_FILES src/*.cxx src/*.cpp)

find_library(GEN_SHARED_LIB NAMES libscene_parser.so HINTS ${BUILD_DIR}/grammar/gen)
find_library(ANTLR_SHARED_LIB NAMES libantlr4-runtime.so HINTS ${ANTLR_DIR}/dist)

add_executable(scene_lang src/main.cpp include/Scene.h)
target_include_directories(scene_lang PUBLIC ${GEN_INCLUDE_DIR} ${ANTLR_INCLUDE_DIR} ${SCENE_LANG_INCLUDE_DIR} ${CIMG_INCLUDE_DIR})
target_link_libraries(scene_lang PUBLIC ${GEN_SHARED_LIB} ${ANTLR_SHARED_LIB})
