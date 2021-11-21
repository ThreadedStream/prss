#include <iostream>
#include <vector>
#include <stack>
#include <optional>
#include <any>

#include <numeric>
#include "antlr4-runtime.h"
#include "Python3Lexer.h"

static constexpr int8_t EOF = (-1);

static int32_t ssa_num = 0;

template<typename T>
using opt = std::optional<T>;

using namespace antlr4;

// TODO(threadedstream): quicky build a lexer and expression flattener


enum class AssignFlag : uint8_t {
    OP_ASSIGN = 0,
};


// AST nodes for P0 subset

struct Node {
    // TODO(threadedstream): fill in the rest

    virtual ~Node(){};
    virtual std::string str() const noexcept {
        return "";
    }
};

struct Module : public Node {
    Module(const std::string &doc, Node *node) : node_(node), doc_(doc) {}

    std::string str() const noexcept override {
        const std::string valid_doc_fmt = doc_ != "" ? doc_ : "None";
        return "Module(" + valid_doc_fmt + ", " + node_->str() + ")";
    }

    Node *node_;
    std::string doc_;
};

struct Stmt : public Node {
    Stmt(const std::vector<Node*>& nodes) : nodes_(nodes) {
    }

    std::string str() const noexcept override {
        std::string stmt_str = "Stmt([";
        for (const auto &node : nodes_) {
            stmt_str += node->str();
        }

        stmt_str += "])";

        return stmt_str;
    }

    std::vector<Node*> nodes_;
};

struct Printnl : public Node {
    Printnl(const std::vector<Node*> &nodes, Node *dest) : nodes_(nodes), dest_(dest) {}

    std::string str() const noexcept override {
        const std::string valid_dest_fmt = dest_ != nullptr ? dest_->str() : "None";
        std::string printnl_str = "Printnl([";
        for (const auto &node : nodes_) {
            printnl_str += node->str();
        }

        printnl_str += "], " + valid_dest_fmt + ")";

        return printnl_str;
    }


    std::vector<Node*> nodes_;
    Node *dest_;
};

struct Assign : public Node {
    Assign(const std::vector<Node*>& nodes, Node *expr) : nodes_(nodes), expr_(expr) {}

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
    Const(std::any value) : value_(value) {}

    std::string str() const noexcept override {
        const int32_t int_val = std::any_cast<int32_t>(value_);
        return "Const(" + std::to_string(int_val) + ")";
    }

    std::any value_;
};

struct Name : public Node {
    Name (const std::string& name) : name_(name) {}

    std::string str() const noexcept override {
        return "Name(" + name_ + ")";
    }
    std::string name_;
};

struct Add : public Node {
    Add (Node* left, Node* right) : left_(left), right_(right) {}

    std::string str() const noexcept override {
        return "Add(" + left_->str() + ", " + right_->str() + ")";
    }

    Node* left_;
    Node* right_;
};

struct UnarySub : public Node {
    UnarySub (Node* expr) : expr_(expr) {}

    std::string str() const noexcept override {
        return "UnarySub(" + expr_->str() + ")";
    }

    Node* expr_;
};

struct CallFunc : public Node {
    CallFunc (Name* name, const std::vector<Node*>& args) : name_(name), args_(args){}

    ~CallFunc() {
        delete name_;
    }

    std::string str() const noexcept override{
        const std::string valid_args_fmt = args_.size() == 0 ? "[]" : ([&] () -> std::string {
            std::string arg_nodes_str = "";
            for (const auto& arg : args_) {
                arg_nodes_str += arg->str();
            }

            return arg_nodes_str;
        })();

        std::string call_func_str = "CallFunc(" + name_->str() + ", " + valid_args_fmt + ")";

        return call_func_str;
    }

    Name* name_;
    std::vector<Node*> args_;
};


// TODO(threadedstream): To be finished. Should really be thinking of better way to write this function
int32_t astNumNodes(Node* node) {
    // mmmm, spaghetti
    if (auto mod = dynamic_cast<Module*>(node)) {
        return 1 + astNumNodes(mod->node_);
    } else if  (auto stmt = dynamic_cast<Stmt*>(node)) {
        std::vector<int32_t> children_node_nums;
        for (const auto &child : stmt->nodes_) {
            children_node_nums.push_back(astNumNodes(child));
        }
        return 1 + std::accumulate(children_node_nums.begin(), children_node_nums.end(), 0);
    } else if (auto name = dynamic_cast<Name*>(node)) {
        return 1;
    } else if (auto printnl = dynamic_cast<Printnl*>(node)) {
        return 1 + astNumNodes(printnl->nodes_[0]);
    } else if (auto assign = dynamic_cast<Assign*>(node)) {
        return 1 + astNumNodes(assign->nodes_[0]) + astNumNodes(assign->expr_);
    } else if (auto ass_name = dynamic_cast<AssName*>(node)) {
        return 1;
    } else if (auto discard = dynamic_cast<Discard*>(node)) {
        return 1 + astNumNodes(discard->expr_);
    } else if (auto cnst = dynamic_cast<Const*>(node)) {
        return 1;
    } else if (auto add = dynamic_cast<Add*>(node)) {
        return 1 + astNumNodes(add->left_) + astNumNodes(add->right_);
    } else if (auto unary_sub = dynamic_cast<UnarySub*>(node)) {
        return 1 + astNumNodes(unary_sub->expr_);
    } else if (auto call_func = dynamic_cast<CallFunc*>(node)) {
        return 1 + astNumNodes(call_func->name_);
    } else {
        throw std::runtime_error("encountered unknown ast node\n");
    }
}

std::string astStr(Node* root) noexcept{
    return root->str();
}

bool isSimpleStatement(const Token* tok, const Python3Lexer& lexer) {
    return (tok->getType() == Python3Lexer::NAME && lexer._input->LA(1) != '(') ||
            (tok->getType() == Python3Lexer::NUMBER);
}

void breakdownComplexStatement(const Token* token) {

}

std::string visitCallFunc(CallFunc* call_func);
std::string visitNode(Node* node);

std::string visitCallFunc(CallFunc* root) {
    std::string code;
    if (!root->args_.size()) {
        ssa_num++;
        return code += "tmp" + std::to_string(ssa_num-1) + " = " + root->name_->name_ + "()";
    } else {
        // reduce arguments
        for (const auto& arg : root->args_) {
            code += visitNode(arg);
            ssa_num++;
        }

        code += "\n" + root->name_->name_ + "(tmp" + std::to_string(ssa_num-1) + ")";
    }

    return code;
}

// useful in more general cases
std::string visitNode(Node* node) {
    std::string code;
    if (const auto call_func = dynamic_cast<CallFunc*>(node)) {
        code += visitCallFunc(call_func);
    } else if (const auto add_expr = dynamic_cast<Add*>(node)){
        code += visitNode(add_expr->left_);
        code += "\ntmp" + std::to_string(ssa_num) + " = " + "tmp" + std::to_string(ssa_num-1) + " + ";
        code += visitNode(add_expr->right_);
    } else if (const auto const_expr = dynamic_cast<Const*>(node)) {
        return std::to_string(std::any_cast<int32_t>(const_expr->value_));
    } else if (const auto unary_sub = dynamic_cast<UnarySub*>(node)) {
        code += visitNode(unary_sub->expr_);
        code += "\ntmp" + std::to_string(ssa_num) + " = -" + "tmp" + std::to_string(ssa_num-1);
        ssa_num++;
    }
    return code;
}

void flattenP0(Node* node) noexcept {
    // TODO(threadedstream): traverse down the whole tree
    int32_t tmp_counter = 0;

    // print(-input() + 2) should take the following form
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

}


int main(int argc, const char* argv[]) {
//    if (argc < 2) {
//        std::puts("Usage: ./<program_name> <path_to_file>\n");
//        return -1;
//    }
//
//    std::ifstream stream(argv[1]);
//    if (!stream) {
//        std::puts("make sure you specified the right path\n");
//        return -1;
//    }
//
//    ANTLRInputStream input_stream(stream);
//
//    Python3Lexer lexer(&input_stream);
//    CommonTokenStream token_stream(&lexer);
//    token_stream.fill();


    std::vector<Node*> print_func_args;
    CallFunc *call_func = new CallFunc(new Name("input"), {});
    UnarySub *unary_sub = new UnarySub(call_func);
    Add *add = new Add(unary_sub, new Const(std::make_any<int32_t>(2)));
    print_func_args.push_back(add);
    CallFunc *print_func = new CallFunc(new Name("print"), print_func_args);
    Stmt *stmt = new Stmt({print_func});
    Module *module = new Module("", stmt);

    const auto code = visitCallFunc(print_func);

    delete call_func;
    delete unary_sub;
    delete add;

    delete print_func;
    delete stmt;
    delete module;

    return 0;
}