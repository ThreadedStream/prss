#include "parser.h"


int main(int argc, const char *argv[]) {
    if (argc < 2) {
        puts("usage: ./program_name <path_to_source>");
        return -1;
    }

    PyLexer lexer(argv[1]);

    auto root = constructAst(lexer);
    const auto ast_str = root->str();

    std::cout << ast_str << '\n';

    return 0;
}