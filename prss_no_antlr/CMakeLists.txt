set(CMAKE_CXX_STANDARD 17)

add_executable(prss_no_antlr main.cpp)
target_include_directories(prss_no_antlr PUBLIC ${ANTLR_INCLUDE_DIR})