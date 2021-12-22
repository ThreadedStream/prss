#include "parser.hpp"


void dumpAntlrAst(const std::string& path) {
    std::ifstream in_stream(path);

    if (!in_stream) {
        return;
    }
    ANTLRInputStream input_stream(in_stream);
    Python3Lexer py_lexer(&input_stream);
    CommonTokenStream tokens(&py_lexer);

    Python3Parser py_parser(&tokens);

    const auto tree = py_parser.atom_expr();

    std::cout << tree->toStringTree(true) << '\n';

    in_stream.close();
}


int main(int argc, const char *argv[]) {
    if (argc < 2) {
        puts("usage: ./program_name <path_to_source>");
        return -1;
    }

    PyLexer lexer(argv[1]);

    auto root = buildAst(lexer);
    //const auto ast_str = root->str();
    dumpAntlrAst("/home/glasser/toys/prss/sources/sample.py");

    destroyAst(root);

    return 0;
}
