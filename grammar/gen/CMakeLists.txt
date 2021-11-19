set(ANTLR_DIR ${CMAKE_SOURCE_DIR}/antlr)
set(ANTLR_INCLUDE_DIR ${ANTLR_DIR}/runtime/src)
set(GEN_INCLUDE_DIR include)

find_library(ANTLR_SHARED_LIB NAMES libantlr4-runtime.so HINTS ${ANTLR_DIR}/dist)

set(HEADER_FILES include/Python3BaseVisitor.h
                 include/Python3Lexer.h
                 include/Python3Parser.h
                 include/Python3Visitor.h
                 include/Python3Listener.h
                 include/Python3BaseListener.h)

set(SOURCE_FILES src/Python3BaseVisitor.cpp
                 src/Python3Lexer.cpp
                 src/Python3Parser.cpp
                 src/Python3Visitor.cpp
                 src/Python3Listener.cpp
                 src/Python3BaseListener.cpp)

add_library(prss_parser_antlr SHARED ${HEADER_FILES} ${SOURCE_FILES})
target_include_directories(prss_parser_antlr PUBLIC ${ANTLR_INCLUDE_DIR} ${GEN_INCLUDE_DIR})
target_link_libraries(prss_parser_antlr PUBLIC ${ANTLR_SHARED_LIB})