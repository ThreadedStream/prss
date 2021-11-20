set(CMAKE_CXX_STANDARD 17)

find_library(ANTLR_SHARED_LIB NAMES libantlr4-runtime.so HINTS ${ANTLR_DIR}/dist)

add_executable(prss_no_antlr main.cpp)
target_include_directories(prss_no_antlr PUBLIC ${ANTLR_INCLUDE_DIR})
target_link_libraries(prss_no_antlr PUBLIC ${ANTLR_SHARED_LIB})