#include <iostream>
#include <vector>
#include <stack>
#include <optional>

#include <cstdint>
#include <numeric>
#include "antlr4-runtime.h"

template<typename T>
using opt<T> = std::optional<T>;

// TODO(threadedstream): quicky build a lexer and expression flattener

// just copied it from Python3Lexer.h
enum class TokenType : uint8_t {
    STRING = 1, NUMBER = 2, INTEGER = 3, DEF = 4, RETURN = 5, RAISE = 6,
    FROM = 7, IMPORT = 8, AS = 9, GLOBAL = 10, NONLOCAL = 11, ASSERT = 12,
    IF = 13, ELIF = 14, ELSE = 15, WHILE = 16, FOR = 17, IN = 18, TRY = 19,
    FINALLY = 20, WITH = 21, EXCEPT = 22, LAMBDA = 23, OR = 24, AND = 25,
    NOT = 26, IS = 27, NONE = 28, TRUE = 29, FALSE = 30, CLASS = 31, YIELD = 32,
    DEL = 33, PASS = 34, CONTINUE = 35, BREAK = 36, ASYNC = 37, AWAIT = 38,
    NEWLINE = 39, NAME = 40, STRING_LITERAL = 41, BYTES_LITERAL = 42, DECIMAL_INTEGER = 43,
    OCT_INTEGER = 44, HEX_INTEGER = 45, BIN_INTEGER = 46, FLOAT_NUMBER = 47,
    IMAG_NUMBER = 48, DOT = 49, ELLIPSIS = 50, STAR = 51, OPEN_PAREN = 52,
    CLOSE_PAREN = 53, COMMA = 54, COLON = 55, SEMI_COLON = 56, POWER = 57,
    ASSIGN = 58, OPEN_BRACK = 59, CLOSE_BRACK = 60, OR_OP = 61, XOR = 62,
    AND_OP = 63, LEFT_SHIFT = 64, RIGHT_SHIFT = 65, ADD = 66, MINUS = 67,
    DIV = 68, MOD = 69, IDIV = 70, NOT_OP = 71, OPEN_BRACE = 72, CLOSE_BRACE = 73,
    LESS_THAN = 74, GREATER_THAN = 75, EQUALS = 76, GT_EQ = 77, LT_EQ = 78,
    NOT_EQ_1 = 79, NOT_EQ_2 = 80, AT = 81, ARROW = 82, ADD_ASSIGN = 83,
    SUB_ASSIGN = 84, MULT_ASSIGN = 85, AT_ASSIGN = 86, DIV_ASSIGN = 87,
    MOD_ASSIGN = 88, AND_ASSIGN = 89, OR_ASSIGN = 90, XOR_ASSIGN = 91, LEFT_SHIFT_ASSIGN = 92,
    RIGHT_SHIFT_ASSIGN = 93, POWER_ASSIGN = 94, IDIV_ASSIGN = 95, SKIP_ = 96,
    UNKNOWN_CHAR = 97
};

enum class AssignFlag : uint8_t {
    OP_ASSIGN = 0,
};

struct Token {
    int32_t line;
    int32_t column;
    TokenType type;
    const char *text;
};

class Lexer {
public:

    Token NextToken() {
        if (LookAhead(1) == EOF && !idents_.empty()) {
            for (auto i = tokens_.size() - 1; i > 0; i--) {

            }
        }
    }


    char LookAhead(int32_t n) {
        if (curr_idx + n < source_.size()) { return EOF; }
        return source_[curr_idx + n];
    }

    Token ProduceToken(const std::string &name, int32_t type) {

    }

private:
    std::vector<Token> tokens_;
    std::stack<int> idents_;
    std::string source_;
    int32_t curr_idx;
};

// AST nodes for P0 subset

struct Node {
    // TODO(threadedstream): fill in the rest

    virtual ~Node();
};

struct Module : public Node {
    Module(const std::string &doc, Node *node) : node_(node), doc_(doc) {}

    Node *node_;
    std::string doc_;
};

struct Stmt : public Node {
    Stmt(std::vector<Node*> nodes) : nodes_(nodes) {
    }

    std::vector<Node*> nodes_;
};

struct Printnl : public Node {
    Printnl(Node *nodes, Node *dest) : nodes_(nodes), dest_(dest) {}

    Node *nodes_;
    Node *dest_;
};

struct Assign : public Node {
    Assign(std::vector<Node*> nodes, Node *expr) : nodes_(nodes), expr_(expr) {}

    std::vector<Node*> nodes_;
    Node *expr_;
};

struct AssName : public Node {
    AssName(const std::string &name, AssignFlag flags) : name_(name), flags_(flags) {}

    std::string name_;
    AssignFlag flags_;
};

struct Discard : public Node {
    Discard(Node *expr) : expr_(expr) {}

    Node *expr_;
};

struct Const : public Node {
    Const(const antlrcpp::Any &value) : value_(value) {}

    antlrcpp::Any value_;
};

struct Name : public Node {
    Name (const std::string& name) : name_(name) {}

    std::string name_;
};

struct Add : public Node {
    Add (Node* left, Node* right) : left_(left), right_(right) {}

    Node* left_;
    Node* right_;
};

struct UnarySub : public Node {
    UnarySub (Node* expr) : expr_(expr) {}

    Node* expr_;
};

struct CallFunc : public Node {
    CallFunc (Node* node, Node* args) : node_(node), args_(args){}

    Node* node_;
    Node* args_;
};


// TODO(threadedstream): To be finished. Should really be thinking of better way to write this function
constexpr int32_t AstNumNodes(Node* node) {
    // convert that to type switch
    if constexpr (auto mod = dynamic_cast<Module*>(node)) {
        return 1 + AstNumNodes(mod->node_);
    } else if constexpr (auto stmt = dynamic_cast<Stmt*>(node)) {
        std::vector<int32_t> children_node_nums;
        for (const auto &node : stmt->nodes_) {
            children_node_nums.push_back(AstNumNodes(node));
        }
        return 1 + std::accumulate(children_node_nums.begin(), children_node_nums.end(), 0);
    }
}
