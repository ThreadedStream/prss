#include <iostream>
#include <filesystem>
#include <Python3BaseListener.h>

#include "antlr4-runtime.h"
#include "Python3Lexer.h"
#include "Python3Listener.h"
#include "Python3Parser.h"
#include "PyParser.h"

using namespace antlr4;

namespace fs = std::filesystem;

int main(int argc, const char* argv[]) {
    if (argc < 2) {
        std::cout << "usage: ./<exe_name> <path_to_py_file>\n";
        return 0;
    }

    const auto path_to_py = argv[1];

    std::ifstream stream(path_to_py, std::ios::in);
    if (!stream) {
        puts("file was not found\n");
        return -1;
    }

    ANTLRInputStream input(stream);
    Python3Lexer lexer(&input);
    CommonTokenStream tokens(&lexer);
    Python3Parser parser(&tokens);

    Python3BaseVisitor visitor;
    auto func_def = visitor.visitXor_expr(parser.xor_expr());
    if (func_def.isNotNull()) {
        const auto str = func_def.as<std::string>();
        std::cout << "Output: " << str << '\n';
    } else {
        return -1;
    }
    tree::ParseTreeWalker tree_walker;
    ParserRuleContext *file_input = parser.file_input();
    Python3BaseListener *listener = new Python3BaseListener();
    tree_walker.walk(listener, file_input);

    return 0;
}
