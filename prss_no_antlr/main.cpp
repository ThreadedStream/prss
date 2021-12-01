#include "parser.hpp"


int main(int argc, const char *argv[]) {
    if (argc < 2) {
        puts("usage: ./program_name <path_to_source>");
        return -1;
    }

    PyLexer lexer(argv[1]);


    auto root = buildAst(lexer);
    const auto ast_str = root->str();

    std::cout << ast_str << '\n';


//    lexer.updateCurr(1);
//    std::cout << "prev: " << lexer.prev << ", ";
//    std::cout << "curr: " << lexer.curr->getType() << ", ";
//    std::cout << "next: " << lexer.next->getType() << '\n';
//
//    lexer.updateCurr(1);
//    std::cout << "prev: " << lexer.prev->getType() << ", ";
//    std::cout << "curr: " << lexer.curr->getType() << ", ";
//    std::cout << "next: " << lexer.next->getType() << '\n';
//
//
//    lexer.updateCurr(1);
//    std::cout << "prev: " << lexer.prev->getType() << ", ";
//    std::cout << "curr: " << lexer.curr->getType() << ", ";
//    std::cout << "next: " << lexer.next->getType() << '\n';
//
//    lexer.backtrack();
//    std::cout << "prev: " << lexer.prev->getType() << ", ";
//    std::cout << "curr: " << lexer.curr->getType() << ", ";
//    std::cout << "next: " << lexer.next->getType() << '\n';

    return 0;
}

#pragma clang diagnostic pop