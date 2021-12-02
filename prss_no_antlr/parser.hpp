#pragma once

#include <iostream>
#include <vector>
#include <stack>
#include <optional>
#include <any>

#include <numeric>
#include "antlr4-runtime.h"
#include "Python3Lexer.h"


#define ADD_OPERANDS_CASE(target, parent, token) \
    case Python3Lexer::NUMBER: \
        (target) = visitConst(token, parent); \
        break;\
    default:                                     \
        const auto next_next_tok = lexer.lookAhead(2);                                         \
        if (token->getType() == Python3Lexer::NAME &&                                     \
        next_next_tok &&                              \
        next_next_tok->getType() == Python3Lexer::OPEN_PAREN) {                                \
            lexer.updateCurr(1);                                     \
            (target) = visitCallFunc(lexer, parent);                                         \
        }                                          \


//    case Python3Lexer::CLOSE_PAREN: \
//        (target) = visitCallFunc(lexer, parent); \


static int32_t ssa_num = 0;

template<typename T>
using opt = std::optional<T>;

using namespace antlr4;

// TODO(threadedstream): quicky build a lexer and expression flattener

enum class AssignFlag : uint8_t {
    OP_ASSIGN = 0,
};

struct Node;
struct Module;
struct CallFunc;
struct BinOp;
struct UnaryOp;
struct Name;
struct Assign;
struct Add;
struct AssName;
struct Discard;
struct Const;


class PyLexer;

int32_t astNumNodes(Node *node);

Node *parseAtomExpr(PyLexer &lexer);
Node *parsePower(PyLexer &lexer);
Node *parseTerm(PyLexer &lexer);
Node *parseArithExpr(PyLexer &lexer);
Node *parseTfpDef(PyLexer& lexer);
Node *parseTypedArgsList(PyLexer& lexer);
Node *parseComparison(PyLexer &lexer);
Node *parseExpr(PyLexer& lexer);
Node *parseStarExpr(PyLexer& lexer);
Node *parseXorExpr(PyLexer& lexer);
Node *parseAndExpr(PyLexer& lexer);
Node *parseShiftExpr(PyLexer& lexer);
Node *parseTest(PyLexer& lexer);
Node *parseTestNoCond(PyLexer& lexer);
Node *parseLambDef(PyLexer& lexer);
Node *parseLambDefNoCond(PyLexer& lexer);
Node *parseOrTest(PyLexer& lexer);
Node *parseAndTest(PyLexer& lexer);
Node *parseArgList(PyLexer& lexer);
Node *parseArgument(PyLexer& lexer);



bool isArithOp(const Token *tok) {
    const auto token_type = tok->getType();
    return token_type == Python3Lexer::ADD || token_type == Python3Lexer::MINUS;
}

bool isTermOp(const Token *tok) {
    const auto token_type = tok->getType();
    return token_type == Python3Lexer::STAR ||
           token_type == Python3Lexer::DIV ||
           token_type == Python3Lexer::IDIV ||
           token_type == Python3Lexer::MOD;
}

bool isCompOp(const Token *tok) {
    const auto token_type = tok->getType();

    return token_type == Python3Lexer::LESS_THAN ||
           token_type == Python3Lexer::GREATER_THAN ||
           token_type == Python3Lexer::EQUALS ||
           token_type == Python3Lexer::GT_EQ ||
           token_type == Python3Lexer::LT_EQ ||
           token_type == Python3Lexer::NOT_EQ_2 ||
           token_type == Python3Lexer::IN ||
           token_type == Python3Lexer::NOT ||
           token_type == Python3Lexer::IS;
}

std::string tokTypeToStr(const int32_t tok_type) {
    switch (tok_type) {
        case Python3Lexer::ADD:
            return "TokAdd";
        case Python3Lexer::MINUS:
            return "TokMinus";
        case Python3Lexer::STAR:
            return "TokMult";
        case Python3Lexer::DIV:
            return "TokDiv";
        case Python3Lexer::IDIV:
            return "TokIntDiv";
        case Python3Lexer::MOD:
            return "TokMod";
        case Python3Lexer::NUMBER:
            return "TokNum";
        case Python3Lexer::STRING:
            return "TokStr";
        case Python3Lexer::XOR:
            return "TokXor";
        case Python3Lexer::OR:
            return "TokOr";
        case Python3Lexer::AND:
            return "TokAnd";
        case Python3Lexer::OR_OP:
            return "TokOrOp";
        case Python3Lexer::AND_OP:
            return "TokAndOp";
        case Python3Lexer::NOT_OP:
            return "TokNotOp";
        case Python3Lexer::NOT:
            return "TokNot";
        default:
            return "TokUnknown";
    }
}

// AST nodes for P0 subset
struct Node {
    // TODO(threadedstream): fill in the rest

    virtual ~Node() {};

    virtual std::string str() const noexcept {
        return "";
    }

    // n - child to add to the parent node
    // next_arg - needed for functions (likely to be removed)
    virtual void addChild(Node *n, const bool next_arg = false) noexcept {}
};

struct Module : public Node {
    Module(const std::string &doc, Node *expr) : expr_(expr), doc_(doc) {}

    std::string str() const noexcept override {
        const std::string valid_doc_fmt = doc_ != "" ? doc_ : "None";
        return "Module(" + valid_doc_fmt + ", " + expr_->str() + ")";
    }

    virtual void addChild(Node *n, const bool) noexcept override {
        expr_ = n;
    }

    Node *expr_;
    std::string doc_;
};

struct Stmt : public Node {
    explicit Stmt(const std::vector<Node *> &nodes) : nodes_(nodes) {
    }

    std::string str() const noexcept override {
        std::string stmt_str = "Stmt([";
        for (const auto &node: nodes_) {
            stmt_str += node->str();
        }

        stmt_str += "])";

        return stmt_str;
    }

    virtual void addChild(Node *n, const bool) noexcept override {
        nodes_.push_back(n);
    }

    std::vector<Node *> nodes_;
};

struct Assign : public Node {
    explicit Assign(const std::vector<Node *> &assignees, Node *assigner) : assignees_(assignees), assigner_(assigner) {}

    std::vector<Node *> assignees_;
    Node *assigner_;
};

struct AssName : public Node {
    explicit AssName(const std::string &name, AssignFlag flags) : name_(name), flags_(flags) {}

    std::string name_;
    AssignFlag flags_;
};

struct Discard : public Node {
    explicit Discard(Node *expr) : expr_(expr) {}

    virtual void addChild(Node *n) noexcept {
        expr_ = n;
    }

    Node *expr_;
};

struct Const : public Node {
    explicit Const(const std::string &value, const int32_t type) : value_(value), type(type) {}

    std::string str() const noexcept override {
        return "Const(value = " + value_ + ", type = " + tokTypeToStr(type) + ")";
    }

    std::string value_;
    int32_t type;
};

struct Name : public Node {
    explicit Name(const std::string &name) : name_(name) {}

    std::string str() const noexcept override {
        return "Name(value = '" + name_ + "')";
    }

    std::string name_;
};

struct BinOp : public Node {
    explicit BinOp(Node *left, Node *right, int32_t op) : left_(left), right_(right), op(op) {}

    std::string str() const noexcept override {
        return "BinOp(left = " + left_->str() + ",right = " + right_->str() + ",op = " + tokTypeToStr(op) + ")";
    }

    Node *left_;
    Node *right_;
    int32_t op;
};

struct UnaryOp : public Node {
    explicit UnaryOp(int32_t op, Node *expr) : op(op), expr_(expr) {}

    std::string str() const noexcept override {
        return "UnarySub(expr = " + expr_->str() +
               ", op = " + tokTypeToStr(op) + ")";
    }

    int32_t op;
    Node *expr_;
};

struct Comparison : public Node {
    explicit Comparison(Node *left, Node *right, const int32_t op) : left(left), right(right), op(op) {}

    std::string str() const noexcept override {
        std::string str = "Compare(left=" + left->str() + ", right=" + right->str() + ", op=" + tokTypeToStr(op);
    }

    Node *left;
    Node *right;
    int32_t op;
};

struct CallFunc : public Node {
    CallFunc(Name *name, const std::vector<Node *> &args) : name_(name), args_(args) {
    }

    ~CallFunc() {
        delete name_;
    }

    std::string str() const noexcept override {
        const std::string valid_args_fmt = args_.size() == 0 ? "[]" : ([&]() -> std::string {
            std::string arg_nodes_str;
            for (const auto &arg: args_) {
                arg_nodes_str += arg->str();
            }

            return arg_nodes_str;
        })();

        std::string call_func_str = "CallFunc(" + name_->str() + ", " + valid_args_fmt + ")";

        return call_func_str;
    }

    virtual void addChild(Node *n, const bool next_arg = false) noexcept override {
        if (args_.empty()) {
            args_.push_back(n);
            return;
        }

        if (!next_arg) {
            args_[curr_arg_num] = n;
        } else {
            args_.push_back(n);
            curr_arg_num++;
        }
    }


    Name *name_;
    std::vector<Node *> args_;
    int32_t curr_arg_num = 0;
};

int32_t sign(int32_t n) {
    return (-1 * (n > 0));
}

class PyLexer {
public:
    explicit PyLexer(const std::string &path) {
        std::ifstream stream(path);
        if (!stream) {
            throw std::runtime_error("path to the source is incorrect");
        }
        ANTLRInputStream input_stream(stream);

        Python3Lexer lexer(&input_stream);
        token_stream = new CommonTokenStream(&lexer);
        token_stream->fill();

        tokens_ = token_stream->getTokens();
        num_of_tokens_ = tokens_.size();
    }

    Token *lookAhead(int32_t n) {
        if (curr_idx_ + n - 1 >= num_of_tokens_ ||
            curr_idx_ + n - 1 < 0) {
            return nullptr;
        }

        return tokens_[curr_idx_ + n - 1];
    }

    void updateCurr(int32_t n) {
        const auto look_ahead_idx = curr_idx_ + n - 1;
        if (look_ahead_idx >= num_of_tokens_ ||
            look_ahead_idx < 0) {

            curr = nullptr;
        }
        prev = curr;
        curr = tokens_[look_ahead_idx];
        curr_idx_ += 1;
        next = lookAhead(1);
        (void) n;
    }

    void consume(int32_t token_type) {
        if (curr->getType() == token_type) {
            updateCurr(1);
        } else {
            // TODO(threadedstream): do cleanup
            throw std::runtime_error("syntax error");
        }
    }

    void backtrack() {
        next = curr;
        curr = prev;
        curr_idx_--;
        prev = lookAhead(-1);
    }

    inline std::vector<Token *> tokens() const noexcept { return tokens_; }

    ~PyLexer() {
        delete token_stream;
    }

public:
    Token *curr;
    Token *next;
    Token *prev;

private:
    CommonTokenStream *token_stream;
    std::vector<Token *> tokens_;
    int32_t num_of_tokens_;
    int32_t curr_idx_ = 0;
};


// print(-input() + 2) should take the following form:
//
// tmp0 = input()
// tmp1 = -tmp0
// tmp2 = tmp1 + 2
// print(tmp2)

// the question here is whether i achieve it using ast or
// solely relying on lexing

// intuition tells me that the former approach is more suitable, as
// process requires an actual separation of complex and simple statements

// simple statements are meant to be the ones involving no operators, function calls, etc.
// for instance, variables and constants are great examples of such expressions.


// factor: ('+'|'-'|'~') factor | power;
Node *parseFactor(PyLexer &lexer) {
    // parsing unary expression
    auto current_token = lexer.curr;
    switch (current_token->getType()) {
        case Python3Lexer::ADD:
        case Python3Lexer::MINUS:
        case Python3Lexer::NOT_OP:
            lexer.consume(current_token->getType());
            return new UnaryOp(current_token->getType(), parseFactor(lexer));
        default:
            return parsePower(lexer);
    }
}

Node *parseArgList(PyLexer& lexer) {
    auto node = parseArgument(lexer);

}


Node *parseTest(PyLexer& lexer) {
    auto node = parseOrTest(lexer);

    if (lexer.curr->getType() == Python3Lexer::IF) {
        lexer.consume(Python3Lexer::IF);
        node = parseOrTest(lexer);
        lexer.consume(Python3Lexer::ELSE);
        node = parseTest(lexer);
    } else if (lexer.curr->getType() == Python3Lexer::LAMBDA) {
        node = parseLambDef(lexer);
    }

    return node;
}

Node *parseTypedArgsList(PyLexer& lexer) {

}

Node *parseExpr(PyLexer& lexer) {
    auto node = parseXorExpr(lexer);

    while (lexer.curr->getType() == Python3Lexer::OR_OP) {
        lexer.consume(Python3Lexer::OR_OP);

        const auto rhs = parseXorExpr(lexer);

        node = new BinOp(node, rhs, Python3Lexer::OR_OP);
    }

    return node;
}

Node *parseXorExpr(PyLexer& lexer) {
    auto node = parseAndExpr(lexer);

    while (lexer.curr->getType() == Python3Lexer::XOR) {
        lexer.consume(Python3Lexer::XOR);

        const auto rhs = parseAndExpr(lexer);

        node = new BinOp(node, rhs, Python3Lexer::XOR);
    }

    return node;
}

Node *parseAndExpr(PyLexer& lexer) {
    auto node = parseShiftExpr(lexer);

    while (lexer.curr->getType() == Python3Lexer::AND_OP) {
        lexer.consume(Python3Lexer::AND_OP);

        const auto rhs = parseShiftExpr(lexer);

        node = new BinOp(node, rhs, Python3Lexer::AND_OP);
    }

    return node;
}

Node *parseShiftExpr(PyLexer& lexer) {
    auto node = parseArithExpr(lexer);

    while (lexer.curr->getType() == Python3Lexer::LEFT_SHIFT ||
            lexer.curr->getType() == Python3Lexer::RIGHT_SHIFT) {
        const auto current_token = lexer.curr;
        lexer.consume(current_token->getType());

        const auto rhs = parseArithExpr(lexer);
        node = new BinOp(node, rhs, current_token->getType());
    }

    return node;
}

Node *parseComparison(PyLexer &lexer) {
    auto node = parseExpr(lexer);

    while (isCompOp(lexer.curr)) {
        const auto current_token = lexer.curr;
        lexer.consume(current_token->getType());

        const auto rhs = parseExpr(lexer);
        node = new Comparison(node, rhs, current_token->getType());
    }

    return node;
}

// power: atom_expr ('**' factor)?;
Node *parsePower(PyLexer &lexer) {
    auto node = parseAtomExpr(lexer);

    while (lexer.curr->getType() == Python3Lexer::POWER) {
        lexer.consume(Python3Lexer::POWER);
        auto rhs = parseFactor(lexer);

        node = new BinOp(node, rhs, Python3Lexer::POWER);
    }

    return node;
}

Node *parseAtomExpr(PyLexer &lexer) {
    const auto current_token = lexer.curr;
    switch (current_token->getType()) {
        // only numbers for now
        case Python3Lexer::NUMBER:
            lexer.consume(Python3Lexer::NUMBER);
            return new Const(current_token->getText(), Python3Lexer::NUMBER);
        default:
            return nullptr;
    }
}

Node *parseTrailer(PyLexer &lexer) {
    const auto current_token = lexer.curr;

    switch (current_token->getType()) {
        case Python3Lexer::OPEN_PAREN:
            lexer.consume(Python3Lexer::OPEN_PAREN);

    }
}

Node *parseTerm(PyLexer &lexer) {
    auto node = parseFactor(lexer);

    while (isTermOp(lexer.curr)) {

        const auto token = lexer.curr;
        switch (token->getType()) {
            case Python3Lexer::STAR:
                lexer.consume(Python3Lexer::STAR);
                break;
            case Python3Lexer::DIV:
                lexer.consume(Python3Lexer::DIV);
                break;
            case Python3Lexer::IDIV:
                lexer.consume(Python3Lexer::IDIV);
                break;
            case Python3Lexer::MOD:
                lexer.consume(Python3Lexer::MOD);
                break;
        }

        const auto rhs = parseFactor(lexer);
        node = new BinOp(node, rhs, token->getType());
    }

    return node;
}

Node *parseArithExpr(PyLexer &lexer) {
    auto node = parseTerm(lexer);

    while (isArithOp(lexer.curr)) {

        const auto token = lexer.curr;
        switch (token->getType()) {
            case Python3Lexer::ADD:
                lexer.consume(Python3Lexer::ADD);
                break;
            case Python3Lexer::MINUS:
                lexer.consume(Python3Lexer::MINUS);
                break;
        }

        node = new BinOp(node, parseTerm(lexer), token->getType());
    }

    return node;
}


Node *buildAst(PyLexer &lexer) {
    lexer.updateCurr(1);
    return parseExpr(lexer);
}


