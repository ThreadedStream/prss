#pragma once

#include <iostream>
#include <vector>
#include <stack>
#include <optional>
#include <any>
#include <sstream>
#include <map>

#include <numeric>
#include "antlr4-runtime.h"
#include "Python3Lexer.h"




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
struct FuncDef;
struct BinOp;
struct UnaryOp;
struct BoolOp;
struct Name;
struct Assign;
struct Add;
struct AssName;
struct Discard;
struct Const;
struct Tfpdef;
struct Argument;
struct Arguments;
struct Test;
struct IfStmt;

class PyLexer;

int32_t astNumNodes(Node *node);

Node *parseAtomExpr(PyLexer &lexer);

Node *parseAtom(PyLexer &lexer);

Node *parsePower(PyLexer &lexer);

Node *parseTerm(PyLexer &lexer);

Node *parseArithExpr(PyLexer &lexer);

Arguments *parseTypedArgsList(PyLexer &lexer);

Node *parseComparison(PyLexer &lexer);

Node *parseExpr(PyLexer &lexer);

Node *parseStarExpr(PyLexer &lexer);

Node *parseXorExpr(PyLexer &lexer);

Node *parseAndExpr(PyLexer &lexer);

Node *parseShiftExpr(PyLexer &lexer);

Node *parseTest(PyLexer &lexer);

Node *parseTestNoCond(PyLexer &lexer);

Node *parseLambDef(PyLexer &lexer);

Node *parseLambDefNoCond(PyLexer &lexer);

Node *parseOrTest(PyLexer &lexer);

Node *parseAndTest(PyLexer &lexer);

Node *parseNotTest(PyLexer &lexer);

Node *parseArgList(PyLexer &lexer);

Argument *parseArgument(PyLexer &lexer);

Arguments *parseParameters(PyLexer &lexer);

FuncDef *parseFuncDef(PyLexer &lexer);

Node *parseSuite(PyLexer &lexer);


std::map<int32_t, std::string> tokTypeToStr = {
        {Python3Lexer::STRING, "TokString"},
        {Python3Lexer::NUMBER, "TokNumber"},
        {Python3Lexer::INTEGER, "TokInteger"},
        {Python3Lexer::DEF, "TokDef"},
        {Python3Lexer::RETURN, "TokReturn"},
        {Python3Lexer::RAISE, "TokRaise"},
        {Python3Lexer::FROM, "TokFrom"},
        {Python3Lexer::IMPORT, "TokImport"},
        {Python3Lexer::AS, "TokAs"},
        {Python3Lexer::GLOBAL, "TokGlobal"},
        {Python3Lexer::NONLOCAL, "TokNonlocal"},
        {Python3Lexer::ASSERT, "TokAssert"},
        {Python3Lexer::IF, "TokIf"},
        {Python3Lexer::ELIF,  "TokElif"},
        {Python3Lexer::ELSE, "TokElse"},
        {Python3Lexer::WHILE, "TokWhile"},
        {Python3Lexer::FOR, "TokFor"},
        {Python3Lexer::IN, "TokIn"},
        {Python3Lexer::TRY, "TokTry"},
        {Python3Lexer::FINALLY, "TokFinally"},
        {Python3Lexer::WITH, "TokWith"},
        {Python3Lexer::EXCEPT, "TokExcept"},
        {Python3Lexer::LAMBDA, "TokLambda"},
        {Python3Lexer::OR, "TokOr"},
        {Python3Lexer::AND, "TokAnd"},
        {Python3Lexer::NOT, "TokNot"},
        {Python3Lexer::IS, "TokIs"},
        {Python3Lexer::NONE, "TokNone"},
        {Python3Lexer::TRUE, "TokTrue"},
        {Python3Lexer::FALSE, "TokFalse"},
        {Python3Lexer::CLASS, "TokClass"},
        {Python3Lexer::YIELD, "TokYield"},
        {Python3Lexer::DEL, "TokDel"},
        {Python3Lexer::PASS, "TokPass"},
        {Python3Lexer::CONTINUE, "TokContinue"},
        {Python3Lexer::BREAK, "TokBreak"},
        {Python3Lexer::ASYNC, "TokAsync"},
        {Python3Lexer::AWAIT, "TokAwait"},
        {Python3Lexer::NEWLINE, "TokNewline"},
        {Python3Lexer::NAME, "TokName"},
        {Python3Lexer::STRING_LITERAL, "TokStringLiteral"},
        {Python3Lexer::BYTES_LITERAL, "TokBytesLiteral"},
        {Python3Lexer::DECIMAL_INTEGER, "TokDecimalInteger"},
        {Python3Lexer::OCT_INTEGER, "TokOctInteger"},
        {Python3Lexer::HEX_INTEGER, "TokHexInteger"},
        {Python3Lexer::BIN_INTEGER, "TokBigInteger"},
        {Python3Lexer::FLOAT_NUMBER, "TokFloatNumber"},
        {Python3Lexer::IMAG_NUMBER, "TokImagNumber"},
        {Python3Lexer::DOT, "TokDot"},
        {Python3Lexer::ELLIPSIS, "TokEllipsis"},
        {Python3Lexer::STAR, "TokStar"},
        {Python3Lexer::OPEN_PAREN, "TokOpenParen"},
        {Python3Lexer::CLOSE_PAREN, "TokCloseParen"},
        {Python3Lexer::COMMA, "TokComma"},
        {Python3Lexer::COLON, "TokColon"},
        {Python3Lexer::SEMI_COLON, "TokSemiColon"},
        {Python3Lexer::POWER, "TokPower"},
        {Python3Lexer::ASSIGN, "TokAssign"},
        {Python3Lexer::OPEN_BRACK, "TokOpenBrack"},
        {Python3Lexer::CLOSE_BRACK, "TokCloseBrack"},
        {Python3Lexer::OR_OP, "TokOrOp"},
        {Python3Lexer::XOR, "TokXor"},
        {Python3Lexer::AND_OP, "TokAndOp"},
        {Python3Lexer::LEFT_SHIFT, "TokLeftShift"},
        {Python3Lexer::RIGHT_SHIFT, "TokRightShift"},
        {Python3Lexer::ADD, "TokAdd"},
        {Python3Lexer::MINUS, "TokMinus"},
        {Python3Lexer::DIV, "TokDiv"},
        {Python3Lexer::MOD, "TokMod"},
        {Python3Lexer::IDIV, "TokIdiv"},
        {Python3Lexer::NOT_OP, "TokNotOp"},
        {Python3Lexer::OPEN_BRACE, "TokOpenBrace"},
        {Python3Lexer::CLOSE_BRACE, "TokCloseBrace"},
        {Python3Lexer::LESS_THAN, "TokLessThan"},
        {Python3Lexer::GREATER_THAN, "TokGreaterThan"},
        {Python3Lexer::EQUALS, "TokEquals"},
        {Python3Lexer::GT_EQ, "TokGtEq"},
        {Python3Lexer::LT_EQ, "TokLtEq"},
        {Python3Lexer::NOT_EQ_1,  "TokNotEq1"},
        {Python3Lexer::NOT_EQ_2, "TokNotEq2"},
        {Python3Lexer::AT, "TokAt"},
        {Python3Lexer::ARROW, "TokArrow"},
        {Python3Lexer::ADD_ASSIGN, "TokAddAssign"},
        {Python3Lexer::SUB_ASSIGN, "TokSubAssign"},
        {Python3Lexer::MULT_ASSIGN, "TokMultAssign"},
        {Python3Lexer::AT_ASSIGN, "TokAtAssign"},
        {Python3Lexer::DIV_ASSIGN, "TokDivAssign"},
        {Python3Lexer::MOD_ASSIGN, "TokModAssign"},
        {Python3Lexer::AND_ASSIGN, "TokAndAssign"},
        {Python3Lexer::OR_ASSIGN, "TokOrAssign"},
        {Python3Lexer::XOR_ASSIGN, "TokXorAssign"},
        {Python3Lexer::LEFT_SHIFT_ASSIGN, "TokLeftShiftAssign"},
        {Python3Lexer::RIGHT_SHIFT_ASSIGN, "TokRightShiftAssign"},
        {Python3Lexer::POWER_ASSIGN, "TokPowerAssign"},
        {Python3Lexer::IDIV_ASSIGN, "TokIDivAssign"},
        {Python3Lexer::SKIP_, "TokSkip"},
        {Python3Lexer::UNKNOWN_CHAR, "TokUnknownChar"}
};

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
        if (curr->getType() == static_cast<size_t>(token_type)) {
            updateCurr(1);
        } else {
            // TODO(threadedstream): do cleanup
            fprintf(stderr, "expected %s", tokTypeToStr[token_type].c_str());
            exit(1);
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

bool isCompOp(PyLexer &lexer) {
    const auto token_type = lexer.curr->getType();
    const auto next_token_type = lexer.next->getType();

    return token_type == Python3Lexer::LESS_THAN ||
           token_type == Python3Lexer::GREATER_THAN ||
           token_type == Python3Lexer::EQUALS ||
           token_type == Python3Lexer::GT_EQ ||
           token_type == Python3Lexer::LT_EQ ||
           token_type == Python3Lexer::NOT_EQ_2 ||
           token_type == Python3Lexer::IN ||
           (token_type == Python3Lexer::NOT && next_token_type == Python3Lexer::IN) ||
           token_type == Python3Lexer::IS ||
           (token_type == Python3Lexer::IS && next_token_type == Python3Lexer::NOT);
}


// AST nodes for P0 subset
struct Node {
    // TODO(threadedstream): fill in the rest

    virtual ~Node() {};

    virtual std::string str() const noexcept {
        return "";
    }

    virtual std::vector<Node *> getChildren() const { return {}; }

    // n - child to add to the parent node
    // next_arg - needed for functions (likely to be removed)
    virtual void addChild(Node *n, const bool next_arg = false) noexcept {}
};

struct Module : public Node {
    Module(const std::string &doc, Node *expr) : expr(expr), doc(doc) {}

    std::string str() const noexcept override {
        std::ostringstream module_str;
        module_str << "Module(doc=" << (doc != "" ? doc : "None") << ",expr=" << expr->str() << ")";
        return module_str.str();
    }

    Node *expr;
    std::string doc;
};

struct Stmt : public Node {
    explicit Stmt(const std::vector<Node *> &nodes)
            : nodes(nodes) {}

    std::string str() const noexcept override {
        std::ostringstream stmt_str;
        stmt_str << "Stmt([";
        for (const auto &node: nodes) {
            stmt_str << node->str();
        }

        stmt_str << "])";

        return stmt_str.str();
    }

    std::vector<Node *> getChildren() const {
        return nodes;
    }

    std::vector<Node *> nodes;
};

struct Test : public Node {
    explicit Test();
};


struct IfStmt : public Node {
    explicit IfStmt(Node *test, Node *body, Node *else_body)
            : test(test), body(body), else_body(else_body) {}

    virtual std::string str() const noexcept override {
        std::ostringstream if_str;
        if_str << "IfStmt(test=" << test->str() << ",body=" << body->str() << ",else_body=" << else_body->str() << ")";
        return if_str.str();
    }

    virtual std::vector<Node *> getChildren() const override {
        return {test, body, else_body};
    }

    Node *test;
    Node *body;
    Node *else_body;
};

struct Assign : public Node {
    explicit Assign(const std::vector<Node *> &assignees, Node *assigner)
            : assignees(assignees), assigner(assigner) {}

    std::vector<Node *> assignees;
    Node *assigner;
};

struct AssName : public Node {
    explicit AssName(const std::string &name, AssignFlag flags)
            : name(name), flags(flags) {}

    std::string name;
    AssignFlag flags;
};

// TODO(threadedstream): What meaning does Discard possess?
struct Discard : public Node {
    explicit Discard(Node *expr)
            : expr(expr) {}

    Node *expr;
};

struct Const : public Node {
    explicit Const(const std::string &value, const int32_t type)
            : value(value), type(type) {}

    std::string str() const noexcept override {
        std::ostringstream const_str;

        const_str << "Const(value=" << value << ",type=" << tokTypeToStr[type];

        return const_str.str();
    }

    virtual std::vector<Node *> getChildren() const override {
        return {};
    }

    std::string value;
    int32_t type;
};

struct Arguments : public Node {
    explicit Arguments(const std::vector<Argument *> &args)
            : args(args) {}


    std::vector<Argument *> args;
};

struct Name : public Node {
    explicit Name(const std::string &name)
            : name(name) {}

    std::string str() const noexcept override {
        return "Name(value = '" + name + "')";
    }

    std::vector<Node *> getChildren() const {
        return {};
    }

    std::string name;
};


struct Argument : public Node {
    explicit Argument(Name *name, Node *type, Node *default_val)
            : name(name), type(type), default_val(default_val) {}

    std::string str() const noexcept override {
        std::ostringstream arg_str;

        arg_str << "Argument(name=" << name->str() << ",type=" << type->str() << ",default_val=" << default_val->str()
                << ")";

        return arg_str.str();
    }

    virtual std::vector<Node *> getChildren() const override {
        return {name, type, default_val};
    }

    Name *name;
    Node *type;
    Node *default_val;
};


struct BinOp : public Node {
    explicit BinOp(Node *left, Node *right, int32_t op) : left(left), right(right), op(op) {}

    std::string str() const noexcept override {
        std::ostringstream bin_op_str;
        bin_op_str << "BinOp(left=" << left->str() << ",right=" << right->str() << ",op=" << tokTypeToStr[op] << ")";
        return bin_op_str.str();
    }

    virtual std::vector<Node *> getChildren() const override {
        return {left, right};
    }

    Node *left;
    Node *right;
    int32_t op;
};

struct UnaryOp : public Node {
    explicit UnaryOp(const int32_t op, Node *expr)
            :  expr(expr), op(op) {}

    std::string str() const noexcept override {
        std::ostringstream unary_op_str;
        unary_op_str << "UnaryOp(expr=" << expr->str() << ",op=" << tokTypeToStr[op] << ")";
        return unary_op_str.str();
    }

    virtual std::vector<Node *> getChildren() const override {
        return {expr};
    }

    Node *expr;
    int32_t op;
};

struct BoolOp : public Node {
    explicit BoolOp(Node *left, Node *right, const int32_t op)
            : left(left), right(right), op(op) {}


    virtual std::string str() const noexcept override {
        std::ostringstream bool_op_str;
        bool_op_str << "BoolOp(left = " << left->str() << ",right=" << right->str() << ",op=" << tokTypeToStr[op]
                    << ")";
        return bool_op_str.str();
    }

    virtual std::vector<Node *> getChildren() const override {
        return {left, right};
    }

    Node *left;
    Node *right;
    int32_t op;
};

struct Comparison : public Node {
    explicit Comparison(Node *left, Node *right, const int32_t op)
            : left(left), right(right), op(op) {}

    std::string str() const noexcept override {
        std::ostringstream comparison_str;
        comparison_str << "Comparison(left=" << left->str() << ",right=" << right->str() << ",op=" << tokTypeToStr[op]
                       << ")";
        return comparison_str.str();
    }

    Node *left;
    Node *right;
    int32_t op;
};

struct FuncDef : public Node {
    explicit FuncDef(Name *name, Arguments *parameters, Node *body, Node *return_type) :
            name(name), parameters(parameters), body(body), return_type(return_type) {};

    std::string str() const noexcept override {
        std::ostringstream func_def_str;
        func_def_str << "FuncDef(name=" << name->str() << ",arguments="
                    << parameters->str() << ",body=" << body->str()
                     << ",return_type=" << return_type->str();

        return func_def_str.str();
    }

    Name *name;
    Arguments *parameters;
    Node *body;
    Node *return_type;
};

struct CallFunc : public Node {
    explicit CallFunc(Name *name, Arguments *arguments)
            : name(name), arguments(arguments) {}

    std::string str() const noexcept override {
        const std::string arguments_str = arguments->args.size() == 0 ? "[]" : ([&]() -> std::string {
            std::string arg_nodes_str;
            for (const auto &arg: arguments->args) {
                arg_nodes_str += arg->str();
            }

            return arg_nodes_str;
        })();

        std::ostringstream call_func_str;
        call_func_str << "CallFunc(name=" << name->str() << ",arguments=" << arguments_str << ")";

        return call_func_str.str();
    }

    std::vector<Node *> getChildren() const override {
        return {name, arguments};
    }

    Name *name;
    Arguments *arguments;
    int32_t curr_arg_num = 0;
};


Arguments *parseParameters(PyLexer &lexer) {
    Arguments *parameters;
    lexer.consume(Python3Lexer::OPEN_PAREN);
    // if function takes some parameters
    if (lexer.curr->getType() != Python3Lexer::CLOSE_PAREN) {
        parameters = parseTypedArgsList(lexer);
    }
    lexer.consume(Python3Lexer::CLOSE_PAREN);
    return parameters;
}

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

// (tfpdef
//      ( ASSIGN test )?
//      ( COMMA tfpdef (ASSIGN test)? )*
//      ( COMMA ( STAR (tfpdef)? (COMMA tfpdef (ASSIGN test)? )*
//      ( COMMA (STAR (tfpdef)? (COMMA tfpdef (ASSIGN test)?)* (COMMA (POWER tfpdef (COMMA)? )?)? |
//      POWER tfpdef (COMMA)?)?)? |
//      STAR (tfpdef)? (COMMA tfpdef (ASSIGN test)?)* (COMMA (POWER tfpdef (COMMA)?)?)? |
//      POWER tfpdef (COMMA)?
// )
Arguments *parseTypedArgsList(PyLexer &lexer) {
    Arguments *arguments = new Arguments({});
    auto argument = parseArgument(lexer);

    if (lexer.curr->getType() == Python3Lexer::ASSIGN) {
        lexer.consume(Python3Lexer::ASSIGN);
        argument->default_val = parseTest(lexer);
    }

    arguments->args.push_back(argument);
    // (',' tfpdef ('=' test)?)*
    // able to parse parameters represented in the following way:
    // (param, param1: type, param2: type = default)
    while (lexer.curr->getType() == Python3Lexer::COMMA &&
           lexer.next->getType() == Python3Lexer::NAME) {
        lexer.consume(Python3Lexer::COMMA);
        auto argument = parseArgument(lexer);
        if (lexer.curr->getType() == Python3Lexer::ASSIGN) {
            lexer.consume(Python3Lexer::ASSIGN);
            argument->default_val = parseTest(lexer);
        }
        arguments->args.push_back(argument);
    }

    return arguments;
}

FuncDef *parseFuncDef(PyLexer &lexer) {
    lexer.consume(Python3Lexer::DEF);

    const auto current_token = lexer.curr;
    lexer.consume(Python3Lexer::NAME);
    auto func_def = new FuncDef(new Name(current_token->getText()), nullptr, nullptr, nullptr);

    auto parameters = parseParameters(lexer);
    func_def->parameters = parameters;

    if (lexer.curr->getType() == Python3Lexer::ARROW) {
        lexer.consume(Python3Lexer::ARROW);
        func_def->return_type = parseTest(lexer);
    }

    lexer.consume(Python3Lexer::COLON);

    func_def->body = parseSuite(lexer);

    return func_def;
}

Node *parseSuite(PyLexer& lexer) {
    return nullptr;
}

Argument *parseArgument(PyLexer &lexer) {
    const auto current_token = lexer.curr;
    lexer.consume(Python3Lexer::Python3Lexer::NAME);

    auto node = new Argument(new Name(current_token->getText()), nullptr, nullptr);
    if (lexer.curr->getType() == Python3Lexer::COLON) {
        lexer.consume(Python3Lexer::COLON);
        node->type = parseTest(lexer);
    }

    return node;
}

Node *parseOrTest(PyLexer &lexer) {
    auto node = parseAndTest(lexer);

    while (lexer.curr->getType() == Python3Lexer::OR) {
        lexer.consume(Python3Lexer::OR);

        const auto rhs = parseAndTest(lexer);
        node = new BoolOp(node, rhs, Python3Lexer::OR);
    }

    return node;
}

Node *parseAndTest(PyLexer &lexer) {
    auto node = parseNotTest(lexer);

    while (lexer.curr->getType() == Python3Lexer::AND) {
        lexer.consume(Python3Lexer::AND);

        const auto rhs = parseNotTest(lexer);
        node = new BoolOp(node, rhs, Python3Lexer::AND);
    }

    return node;
}

Node *parseNotTest(PyLexer &lexer) {
    Node *node;
    switch (lexer.curr->getType()) {
        case Python3Lexer::NOT: {
            lexer.consume(Python3Lexer::NOT);
            const auto expr = parseNotTest(lexer);
            node = new UnaryOp(Python3Lexer::NOT, expr);
        }
            break;
        default:
            node = parseComparison(lexer);
    }

    return node;
}


Node *parseLambDef(PyLexer &lexer) {
    return nullptr;
}

Node *parseTest(PyLexer &lexer) {
    auto node = parseOrTest(lexer);

    switch (lexer.curr->getType()) {
        case Python3Lexer::IF: {
            lexer.consume(Python3Lexer::IF);
            const auto if_test = parseOrTest(lexer);
            lexer.consume(Python3Lexer::ELSE);
            const auto else_body = parseTest(lexer);
            node = new IfStmt(if_test, node, else_body);
        }break;
        case Python3Lexer::LAMBDA:
            node = parseLambDef(lexer);
            break;
    }

    return node;
}

Node *parseExpr(PyLexer &lexer) {
    auto node = parseXorExpr(lexer);

    while (lexer.curr->getType() == Python3Lexer::OR_OP) {
        lexer.consume(Python3Lexer::OR_OP);

        const auto rhs = parseXorExpr(lexer);

        node = new BinOp(node, rhs, Python3Lexer::OR_OP);
    }

    return node;
}

Node *parseXorExpr(PyLexer &lexer) {
    auto node = parseAndExpr(lexer);

    while (lexer.curr->getType() == Python3Lexer::XOR) {
        lexer.consume(Python3Lexer::XOR);

        const auto rhs = parseAndExpr(lexer);

        node = new BinOp(node, rhs, Python3Lexer::XOR);
    }

    return node;
}

Node *parseAndExpr(PyLexer &lexer) {
    auto node = parseShiftExpr(lexer);

    while (lexer.curr->getType() == Python3Lexer::AND_OP) {
        lexer.consume(Python3Lexer::AND_OP);

        const auto rhs = parseShiftExpr(lexer);

        node = new BinOp(node, rhs, Python3Lexer::AND_OP);
    }

    return node;
}

Node *parseShiftExpr(PyLexer &lexer) {
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

    while (isCompOp(lexer)) {
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
    return parseAtom(lexer);
}

Node *parseAtom(PyLexer &lexer) {
    const auto current_token = lexer.curr;
    switch (current_token->getType()) {
        // only numbers for now
        case Python3Lexer::NUMBER:
            lexer.consume(Python3Lexer::NUMBER);
            return new Const(current_token->getText(), Python3Lexer::NUMBER);
        case Python3Lexer::STRING:
            lexer.consume(Python3Lexer::STRING);
            return new Const(current_token->getText(), Python3Lexer::STRING);
        case Python3Lexer::NAME:
            lexer.consume(Python3Lexer::NAME);
            return new Name(current_token->getText());
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

    return nullptr;
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
    return parseFuncDef(lexer);
}

void destroyAst(Node *root) {
    if (!root) return;
    if (root->getChildren().empty()) {
        delete root;
    } else {
        // destroy all children
        for (const auto &child: root->getChildren()) {
            destroyAst(child);
        }

        // and only then destroy the parent
        delete root;
    }
}


