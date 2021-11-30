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
struct UnarySub;
struct Name;
struct Assign;
struct Add;
struct AssName;
struct Discard;
struct Const;
// this one's likely to be removed
struct Printnl;


class PyLexer;

// visitors
CallFunc *visitCallFunc(PyLexer &lexer, Node *&parent);

Add *visitAdd(PyLexer &lexer, Node *&parent);

Assign *visitAssign(PyLexer &lexer, Node *&parent);

int32_t astNumNodes(Node *node);

std::string flattenCallFunc(CallFunc *call_func);

std::string flattenNode(Node *node);


Node *parseFuncCall(PyLexer &lexer);

Node *parseLiteral(PyLexer &lexer);

Node *parseIdentifier(PyLexer &lexer);

Node *parseBinOp(PyLexer &lexer);

Node *parseExpression(PyLexer &lexer);


static Node *current_parent = nullptr;
static Node *prev_parent = nullptr;

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
    Stmt(const std::vector<Node *> &nodes) : nodes_(nodes) {
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

struct Printnl : public Node {
    Printnl(const std::vector<Node *> &nodes, Node *dest) : nodes_(nodes), dest_(dest) {}

    std::string str() const noexcept override {
        const std::string valid_dest_fmt = dest_ != nullptr ? dest_->str() : "None";
        std::string printnl_str = "Printnl([";
        for (const auto &node: nodes_) {
            printnl_str += node->str();
        }

        printnl_str += "], " + valid_dest_fmt + ")";

        return printnl_str;
    }


    std::vector<Node *> nodes_;
    Node *dest_;
};

struct Assign : public Node {
    Assign(const std::vector<Node *> &assignees, Node *assigner) : assignees_(assignees), assigner_(assigner) {}

    std::vector<Node *> assignees_;
    Node *assigner_;
};

struct AssName : public Node {
    AssName(const std::string &name, AssignFlag flags) : name_(name), flags_(flags) {}

    std::string name_;
    AssignFlag flags_;
};

struct Discard : public Node {
    Discard(Node *expr) : expr_(expr) {}

    virtual void addChild(Node *n) noexcept {
        expr_ = n;
    }

    Node *expr_;
};

struct Const : public Node {
    Const(std::any value) : value_(value) {}

    std::string str() const noexcept override {
        const int64_t int_val = std::any_cast<int64_t>(value_);
        return "Const(" + std::to_string(int_val) + ")";
    }

    std::any value_;
};

struct Name : public Node {
    Name(const std::string &name) : name_(name) {}

    std::string str() const noexcept override {
        return "Name(" + name_ + ")";
    }

    std::string name_;
};

struct BinOp : public Node {
    BinOp(Node *left, Node *right, int32_t op) : left_(left), right_(right), op(op) {}

    std::string str() const noexcept override {
        return "BinOp(" + left_->str() + ", " + right_->str() + ")";
    }

    Node *left_;
    Node *right_;
    int32_t op;
};

struct UnarySub : public Node {
    UnarySub(Node *expr) : expr_(expr) {}

    std::string str() const noexcept override {
        return "UnarySub(" + expr_->str() + ")";
    }

    Node *expr_;
};

struct CallFunc : public Node {
    CallFunc(Name *name, const std::vector<Node *> &args) : name_(name), args_(args) {
    }

    ~CallFunc() {
        delete name_;
    }

    std::string str() const noexcept override {
        const std::string valid_args_fmt = args_.size() == 0 ? "[]" : ([&]() -> std::string {
            std::string arg_nodes_str = "";
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


//// TODO(threadedstream): To be finished. Should really be thinking of the way to give that function a better look
//int32_t astNumNodes(Node *node) {
//    // mmmm, spaghetti
//    if (auto mod = dynamic_cast<Module *>(node)) {
//        return 1 + astNumNodes(mod->expr_);
//    } else if (auto stmt = dynamic_cast<Stmt *>(node)) {
//        std::vector<int32_t> children_node_nums;
//        for (const auto &child: stmt->nodes_) {
//            children_node_nums.push_back(astNumNodes(child));
//        }
//        return 1 + std::accumulate(children_node_nums.begin(), children_node_nums.end(), 0);
//    } else if (auto name = dynamic_cast<Name *>(node)) {
//        return 1;
//    } else if (auto printnl = dynamic_cast<Printnl *>(node)) {
//        return 1 + astNumNodes(printnl->nodes_[0]);
//    } else if (auto assign = dynamic_cast<Assign *>(node)) {
//        return 1 + astNumNodes(assign->assignees_[0]) + astNumNodes(assign->assigner_);
//    } else if (auto ass_name = dynamic_cast<AssName *>(node)) {
//        return 1;
//    } else if (auto discard = dynamic_cast<Discard *>(node)) {
//        return 1 + astNumNodes(discard->expr_);
//    } else if (auto cnst = dynamic_cast<Const *>(node)) {
//        return 1;
//    } else if (auto add = dynamic_cast<Add *>(node)) {
//        return 1 + astNumNodes(add->left_) + astNumNodes(add->right_);
//    } else if (auto unary_sub = dynamic_cast<UnarySub *>(node)) {
//        return 1 + astNumNodes(unary_sub->expr_);
//    } else if (auto call_func = dynamic_cast<CallFunc *>(node)) {
//        return 1 + astNumNodes(call_func->name_);
//    } else {
//        throw std::runtime_error("encountered unknown ast node\n");
//    }
//}

std::string astStr(Node *root) noexcept {
    return root->str();
}

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

bool isOperator(const Token *tok) {
    return tok->getType() == Python3Lexer::ADD || tok->getType() == Python3Lexer::MINUS;
}

Node *parseFuncCall(PyLexer &lexer) {
    auto function_call = new CallFunc(new Name(lexer.curr->getText()), {});
    lexer.updateCurr(1);
    lexer.updateCurr(1);
    // parse function parameters
    std::cout << lexer.curr->getText() << '\n';

    Node *arg_expr = nullptr;
    while (lexer.curr->getType() != Python3Lexer::CLOSE_PAREN) {
        arg_expr = parseExpression(lexer);
        lexer.updateCurr(1);
    }

    function_call->args_.push_back(arg_expr);

    return function_call;
}


Node *parseIdentifier(PyLexer &lexer) {
    const auto next_token = lexer.lookAhead(1);
    if (next_token->getType() == Python3Lexer::OPEN_PAREN) {
        return parseFuncCall(lexer);
    }
    // replace it by instantiating Name object
    return nullptr;
}

Node *parseLiteral(PyLexer &lexer) {
    Const *lit;
    switch (lexer.curr->getType()) {
        case Python3Lexer::STRING:
            lit = new Const(std::make_any<std::string>(lexer.curr->getText()));
        case Python3Lexer::NUMBER:
            lit = new Const(std::make_any<int64_t>(std::atol(lexer.curr->getText().c_str())));
    }
    lexer.updateCurr(1);
    return lit;
}

Node *parseBinOp(PyLexer &lexer) {
    auto bin_op = new BinOp(nullptr, nullptr, -1);

    // LIT { ( op LIT ) }
    bin_op->left_ = parseExpression(lexer);
    bin_op->op = lexer.curr->getType();

    lexer.updateCurr(1);
    bin_op->right_ = parseExpression(lexer);

    return bin_op;
}

// expr -> sum
// sum -> digit { (op digit) }

// 1 + 2 + 3 + 4
Node *parseExpression(PyLexer &lexer) {
    switch (lexer.curr->getType()) {
        case Python3Lexer::ADD:
        case Python3Lexer::MINUS:
            return parseBinOp(lexer);
        default:
            if (lexer.curr->getType() == Python3Lexer::STRING ||
                lexer.curr->getType() == Python3Lexer::NUMBER ||
                lexer.curr->getType() == Python3Lexer::NAME) {
                if (lexer.next && isOperator(lexer.next)) {
                    return parseBinOp(lexer);
                }
                switch (lexer.curr->getType()) {
                    case Python3Lexer::STRING:
                    case Python3Lexer::NUMBER:
                        return parseLiteral(lexer);
                    case Python3Lexer::NAME:
                        return parseIdentifier(lexer);
                }
            }
    }
}

Node *buildAst(PyLexer &lexer) {
    Module *module = new Module("", nullptr);
    lexer.updateCurr(1);
    if (lexer.curr->getType() == Python3Lexer::NEWLINE) {
        lexer.updateCurr(1);
    }

    if (lexer.curr->getType() != Python3Lexer::EOF) {
        module->expr_ = parseExpression(lexer);
    }

    return module;
}


//std::string flattenCallFunc(CallFunc *root) {
//    std::string code;
//    if (!root->args_.empty()) {
//        ssa_num++;
//        return code += "\ntmp" + std::to_string(ssa_num - 1) + " = " + root->name_->name_ + "()";
//    } else {
//        // reduce arguments
//        for (const auto &arg: root->args_) {
//            code += flattenNode(arg);
//            ssa_num++;
//        }
//
//        code += "\n" + root->name_->name_ + "(tmp" + std::to_string(ssa_num - 1) + ")";
//    }
//
//    return code;
//}

//// useful in more general cases
//std::string flattenNode(Node *node) {
//    std::string code;
//    if (const auto call_func = dynamic_cast<CallFunc *>(node)) {
//        code += flattenCallFunc(call_func);
//    } else if (const auto add_expr = dynamic_cast<Add *>(node)) {
//        code += flattenNode(add_expr->left_);
//        code += flattenNode(add_expr->right_);
//        code += "\ntmp" + std::to_string(ssa_num) + " = " +
//                "tmp" + std::to_string(ssa_num - 2) + " + " +
//                "tmp" + std::to_string(ssa_num - 1);
//    } else if (const auto const_expr = dynamic_cast<Const *>(node)) {
//        return std::to_string(std::any_cast<int64_t>(const_expr->value_));
//    } else if (const auto unary_sub = dynamic_cast<UnarySub *>(node)) {
//        code += flattenNode(unary_sub->expr_);
//        code += "\ntmp" + std::to_string(ssa_num) + " = -" + "tmp" + std::to_string(ssa_num - 1);
//        ssa_num++;
//    }
//    return code;
//}


Const *visitConst(Token *tok, Node *&parent) {
    auto const_val = new Const(std::make_any<int64_t>(
                                       std::atol(tok->getText().c_str())
                               )
    );

    parent->addChild(const_val);

    return const_val;
}

//Add *visitAdd(PyLexer &lexer, Node *&parent) {
//    auto add = new Add(nullptr, nullptr);
//    // get left operand
//    if (dynamic_cast<Add*>(prev_parent)) {
//        add->left_ = prev_parent;
//    } else {
//        auto prev_token = lexer.lookAhead(-1);
//        if (prev_token) {
//            switch (prev_token->getType()) {
//                ADD_OPERANDS_CASE(add->left_, reinterpret_cast<Node *&>(add), prev_token)
//            }
//        }
//    }
//
//    const auto next_token = lexer.lookAhead(1);
//    if (next_token) {
//        switch (next_token->getType()) {
//            ADD_OPERANDS_CASE(add->right_, reinterpret_cast<Node *&>(add), next_token)
//        }
//    }
//
//    lexer.updateCurr(1);
//    lexer.updateCurr(1);
//    parent->addChild(add);
//
//    return add;
//}



//CallFunc *visitCallFunc(PyLexer &lexer, Node *&parent) {
//    // no arguments by default
//    CallFunc *call_func = new CallFunc(new Name(lexer.curr->getText()), {});
//    lexer.updateCurr(1);
//    lexer.updateCurr(1);
//
//    current_parent = call_func;
//    while (lexer.curr->getType() != Python3Lexer::CLOSE_PAREN && lexer.curr->getType() != Python3Lexer::EOF) {
//        std::cout << lexer.curr->getText() << '\n';
//        // function has some arguments
//        switch (lexer.curr->getType()) {
//            case Python3Lexer::NAME: {
//                const auto next_token = lexer.lookAhead(1);
//                if (next_token && next_token->getType() == Python3Lexer::OPEN_PAREN) {
//                    auto call_func_node = visitCallFunc(lexer, current_parent);
//                    current_parent->addChild(call_func_node);
//                    prev_parent = current_parent;
//                    current_parent = call_func_node;
//                    lexer.updateCurr(1);
//                }
//            }break;
//            case Python3Lexer::ADD: {
//                auto add_node = visitAdd(lexer, current_parent);
//                prev_parent = add_node;
//                current_parent = call_func;
//            }break;
//            default: {
//                auto next_tok = lexer.lookAhead(1);
//                if (next_tok && next_tok->getType() == Python3Lexer::ADD) {
//                    lexer.updateCurr(1);
//                    auto add_node = visitAdd(lexer, current_parent);
//                    prev_parent = add_node;
//                    current_parent = call_func;
//                }
//            }break;
//        }
//    }
//
//    parent->addChild(call_func);
//    return call_func;
//}

//Assign *visitAssign(PyLexer &lexer) {
//
//}


//Node* constructAst(PyLexer &lexer) {
//    static auto module = new Module("", nullptr);
//    lexer.updateCurr(1);
//    std::cout << lexer.curr->getType() << '\n';
//    const auto next_token = lexer.lookAhead(1);
//    if (!next_token) {
//        return nullptr;
//    }
//    std::cout << next_token->getType() << '\n';
//    if (lexer.curr->getType() == Python3Lexer::NAME) {
//        if (next_token->getType() == Python3Lexer::OPEN_PAREN) {
//            Node *stmt = new Stmt({});
//            visitCallFunc(lexer, stmt);
//            module->expr_ = stmt;
//        } else if (next_token->getType() == Python3Lexer::ASSIGN) {
//            // TODO(threadedstream): handle assignment
//            Assign *assign = visitAssign(lexer);
//        }
//    } else if (lexer.curr->getType() == Python3Lexer::NEWLINE) {
//        constructAst(lexer);
//    }
//
//    return module;
//}

