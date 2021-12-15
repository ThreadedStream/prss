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

#define ERR_TOK(expected_token_type) fprintf(stderr, "expected token %d", expected_token_type)
#define ERR_MSG(message) fprintf(stderr, message)
#define ERR_MSG_EXIT(message) ERR_MSG(message); exit(1)
//#define ERR_MSG_CLEANUP_EXIT(message) ERR_MSG(message); destroyAst()

template<typename T>
using opt = std::optional<T>;

using namespace antlr4;

// TODO(threadedstream): quicky build a lexer and expression flattener

enum class AssignFlag : uint8_t {
    OP_ASSIGN = 0,
};

struct Node;
struct Module;
struct SimpleStmt;
struct FuncDef;
struct Lambda;
struct ClassDef;
struct AsyncFuncDef;
struct BinOp;
struct UnaryOp;
struct BoolOp;
struct Name;
struct Assign;
struct AnnAssign;
struct GeneratorExp;
struct StarredExpr;
struct Pass;
struct Yield;
struct Call;
struct Comprehension;
struct YieldFrom;
struct ExprList;
struct Keyword;
struct Import;
struct ImportFrom;
struct Raise;
struct Break;
struct Alias;
struct Aliases;
struct Continue;
struct Attribute;
struct TestList;
struct Return;
struct Const;
struct Argument;
struct Arguments;
struct DictComp;
struct Parameter;
struct Parameters;
struct SetComp;
struct Dict;
struct Set;
struct Global;
struct Assert;
struct Nonlocal;
struct IfStmt;
struct WhileStmt;
struct ForStmt;
struct TryStmt;
struct WithStmt;

struct Delete;

class PyLexer;

// TODO(threadedstream): This is a quick note on how to parse the terminal COMMA symbols taking place after series of exprs separated by comma
// For instance, let's take the following rule: (expr|star_expr) (',' (expr|star_expr))* (',')?
// Here, expr or star_expr is followed by series of exprs or star_exprs separated by comma.
// The problem is that there's a terminal comma located at the end of the rule.
// I guess this comma could be easily caught should i added more verbosity when parsing
// abovementioned sequence, i.e there, in a loop, should be a gigantic (or not so) switch statement
// deciding which way to go next, and breaking the loop in case if no pattern was found, i.e executing "default:" branch.


int32_t astNumNodes(Node *node);

Node *parseAtomExpr(PyLexer &lexer);

Node *parseAtom(PyLexer &lexer);

Node *parsePower(PyLexer &lexer);

Node *parseTerm(PyLexer &lexer);

Node *parseArithExpr(PyLexer &lexer);

Node *parseVarArgsList(PyLexer &lexer);

Parameters *parseTypedArgsList(PyLexer &lexer);

Node *parseComparison(PyLexer &lexer);

Node *parseExpr(PyLexer &lexer);

ExprList *parseExprList(PyLexer &lexer);

StarredExpr *parseStarExpr(PyLexer &lexer);

Node *parseXorExpr(PyLexer &lexer);

Node *parseAndExpr(PyLexer &lexer);

Node *parseShiftExpr(PyLexer &lexer);

Node *parseTest(PyLexer &lexer);

Node *parseTestNoCond(PyLexer &lexer);

TestList *parseTestlistStarExpr(PyLexer &lexer);

Node *parseLambDef(PyLexer &lexer);

Node *parseLambDefNoCond(PyLexer &lexer);

Node *parseOrTest(PyLexer &lexer);

Node *parseAndTest(PyLexer &lexer);

Node *parseNotTest(PyLexer &lexer);

Arguments *parseArglist(PyLexer &lexer);

Node *parseArgument(PyLexer &lexer);

Parameters *parseParameters(PyLexer &lexer);

Parameter *parseParameter(PyLexer &lexer, const bool check_type);

Node *parseAsyncStmt(PyLexer &lexer);

FuncDef *parseFuncDef(PyLexer &lexer);

ClassDef *parseClassDef(PyLexer &lexer);

Node *parseSuite(PyLexer &lexer);

Node *parseStmt(PyLexer &lexer);

Node *parseSimpleStmt(PyLexer &lexer);

Node *parseImportStmt(PyLexer &lexer);

Import *parseImportName(PyLexer &lexer);

Node *parseTestNoCond(PyLexer &lexer);

Node *parseTestlistComp(PyLexer &lexer);

Node *parseCompoundStmt(PyLexer &lexer);

ImportFrom *parseImportFrom(PyLexer &lexer);

Alias *parseImportAsName(PyLexer &lexer);

IfStmt *parseIfStmt(PyLexer &lexer, int32_t depth);

WhileStmt *parseWhileStmt(PyLexer &lexer);

WithStmt *parseWithStmt(PyLexer &lexer);

ForStmt *parseForStmt(PyLexer &lexer);

TryStmt *parseTryStmt(PyLexer &lexer);

Aliases *parseImportAsNames(PyLexer &lexer);

Raise *parseRaiseStmt(PyLexer &lexer);

Node *parseYieldStmt(PyLexer &lexer);

Node *parseYieldExpr(PyLexer &lexer);

Node *parseExprStmt(PyLexer &lexer);

Node *parseCompIter(PyLexer &lexer);

Node *parseCompFor(PyLexer &lexer);

Node *parseCompIf(PyLexer &lexer);

Node *parseDictorsetmaker(PyLexer &lexer);

AnnAssign *parseAnnAssign(PyLexer &lexer);

Delete *parseDelStmt(PyLexer &lexer);

Pass *parsePassStmt(PyLexer &lexer);

Name *parseDottedName(PyLexer &lexer);

Node *parseDecorated(PyLexer &lexer);

Node *parseDict(PyLexer &lexer);

Node *parseSet(PyLexer &lexer);

Alias *parseDottedAsName(PyLexer &lexer);

Aliases *parseDottedAsNames(PyLexer &lexer);

Break *parseBreakStmt(PyLexer &lexer);

Continue *parseContinueStmt(PyLexer &lexer);

Return *parseReturnStmt(PyLexer &lexer);

Node *parseFlowStmt(PyLexer &lexer);

Node *parseImportStmt(PyLexer &lexer);

Global *parseGlobalStmt(PyLexer &lexer);

Nonlocal *parseNonlocalStmt(PyLexer &lexer);

Assert *parseAssertStmt(PyLexer &lexer);

Node *parseSmallStmt(PyLexer &lexer);

TestList *parseTestlist(PyLexer &lexer);

Node *buildAst(PyLexer &lexer);

void destroyAst(Node *root);

namespace tok_utils {
    static std::map<int32_t, std::string> tokTypeToStr = {
            {Python3Parser::STRING,             "TokString"},
            {Python3Parser::NUMBER,             "TokNumber"},
            {Python3Parser::INTEGER,            "TokInteger"},
            {Python3Parser::DEF,                "TokDef"},
            {Python3Parser::RETURN,             "TokReturn"},
            {Python3Parser::RAISE,              "TokRaise"},
            {Python3Parser::FROM,               "TokFrom"},
            {Python3Parser::IMPORT,             "TokImport"},
            {Python3Parser::AS,                 "TokAs"},
            {Python3Parser::GLOBAL,             "TokGlobal"},
            {Python3Parser::NONLOCAL,           "TokNonlocal"},
            {Python3Parser::ASSERT,             "TokAssert"},
            {Python3Parser::IF,                 "TokIf"},
            {Python3Parser::ELIF,               "TokElif"},
            {Python3Parser::ELSE,               "TokElse"},
            {Python3Parser::WHILE,              "TokWhile"},
            {Python3Parser::FOR,                "TokFor"},
            {Python3Parser::IN,                 "TokIn"},
            {Python3Parser::TRY,                "TokTry"},
            {Python3Parser::FINALLY,            "TokFinally"},
            {Python3Parser::WITH,               "TokWith"},
            {Python3Parser::EXCEPT,             "TokExcept"},
            {Python3Parser::LAMBDA,             "TokLambda"},
            {Python3Parser::OR,                 "TokOr"},
            {Python3Parser::AND,                "TokAnd"},
            {Python3Parser::NOT,                "TokNot"},
            {Python3Parser::IS,                 "TokIs"},
            {Python3Parser::NONE,               "TokNone"},
            {Python3Parser::TRUE,               "TokTrue"},
            {Python3Parser::FALSE,              "TokFalse"},
            {Python3Parser::CLASS,              "TokClass"},
            {Python3Parser::YIELD,              "TokYield"},
            {Python3Parser::DEL,                "TokDel"},
            {Python3Parser::PASS,               "TokPass"},
            {Python3Parser::CONTINUE,           "TokContinue"},
            {Python3Parser::BREAK,              "TokBreak"},
            {Python3Parser::ASYNC,              "TokAsync"},
            {Python3Parser::AWAIT,              "TokAwait"},
            {Python3Parser::NEWLINE,            "TokNewline"},
            {Python3Parser::NAME,               "TokName"},
            {Python3Parser::STRING_LITERAL,     "TokStringLiteral"},
            {Python3Parser::BYTES_LITERAL,      "TokBytesLiteral"},
            {Python3Parser::DECIMAL_INTEGER,    "TokDecimalInteger"},
            {Python3Parser::OCT_INTEGER,        "TokOctInteger"},
            {Python3Parser::HEX_INTEGER,        "TokHexInteger"},
            {Python3Parser::BIN_INTEGER,        "TokBigInteger"},
            {Python3Parser::FLOAT_NUMBER,       "TokFloatNumber"},
            {Python3Parser::IMAG_NUMBER,        "TokImagNumber"},
            {Python3Parser::DOT,                "TokDot"},
            {Python3Parser::ELLIPSIS,           "TokEllipsis"},
            {Python3Parser::STAR,               "TokStar"},
            {Python3Parser::OPEN_PAREN,         "TokOpenParen"},
            {Python3Parser::CLOSE_PAREN,        "TokCloseParen"},
            {Python3Parser::COMMA,              "TokComma"},
            {Python3Parser::COLON,              "TokColon"},
            {Python3Parser::SEMI_COLON,         "TokSemiColon"},
            {Python3Parser::POWER,              "TokPower"},
            {Python3Parser::ASSIGN,             "TokAssign"},
            {Python3Parser::OPEN_BRACK,         "TokOpenBrack"},
            {Python3Parser::CLOSE_BRACK,        "TokCloseBrack"},
            {Python3Parser::OR_OP,              "TokOrOp"},
            {Python3Parser::XOR,                "TokXor"},
            {Python3Parser::AND_OP,             "TokAndOp"},
            {Python3Parser::LEFT_SHIFT,         "TokLeftShift"},
            {Python3Parser::RIGHT_SHIFT,        "TokRightShift"},
            {Python3Parser::ADD,                "TokAdd"},
            {Python3Parser::MINUS,              "TokMinus"},
            {Python3Parser::DIV,                "TokDiv"},
            {Python3Parser::MOD,                "TokMod"},
            {Python3Parser::IDIV,               "TokIdiv"},
            {Python3Parser::NOT_OP,             "TokNotOp"},
            {Python3Parser::OPEN_BRACE,         "TokOpenBrace"},
            {Python3Parser::CLOSE_BRACE,        "TokCloseBrace"},
            {Python3Parser::LESS_THAN,          "TokLessThan"},
            {Python3Parser::GREATER_THAN,       "TokGreaterThan"},
            {Python3Parser::EQUALS,             "TokEquals"},
            {Python3Parser::GT_EQ,              "TokGtEq"},
            {Python3Parser::LT_EQ,              "TokLtEq"},
            {Python3Parser::NOT_EQ_1,           "TokNotEq1"},
            {Python3Parser::NOT_EQ_2,           "TokNotEq2"},
            {Python3Parser::AT,                 "TokAt"},
            {Python3Parser::ARROW,              "TokArrow"},
            {Python3Parser::ADD_ASSIGN,         "TokAddAssign"},
            {Python3Parser::SUB_ASSIGN,         "TokSubAssign"},
            {Python3Parser::MULT_ASSIGN,        "TokMultAssign"},
            {Python3Parser::AT_ASSIGN,          "TokAtAssign"},
            {Python3Parser::DIV_ASSIGN,         "TokDivAssign"},
            {Python3Parser::MOD_ASSIGN,         "TokModAssign"},
            {Python3Parser::AND_ASSIGN,         "TokAndAssign"},
            {Python3Parser::OR_ASSIGN,          "TokOrAssign"},
            {Python3Parser::XOR_ASSIGN,         "TokXorAssign"},
            {Python3Parser::LEFT_SHIFT_ASSIGN,  "TokLeftShiftAssign"},
            {Python3Parser::RIGHT_SHIFT_ASSIGN, "TokRightShiftAssign"},
            {Python3Parser::POWER_ASSIGN,       "TokPowerAssign"},
            {Python3Parser::IDIV_ASSIGN,        "TokIDivAssign"},
            {Python3Parser::SKIP_,              "TokSkip"},
            {Python3Parser::UNKNOWN_CHAR,       "TokUnknownChar"}
    };

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
        if (curr->getType() == static_cast<size_t>(token_type)) {
            updateCurr(1);
        } else {
            // TODO(threadedstream): do cleanup
            fprintf(stderr, "expected %s", tok_utils::tokTypeToStr[token_type].c_str());
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


inline bool isAugAssign(const Token *tok) {
    const auto token_type = tok->getType();

    return token_type == Python3Parser::ADD_ASSIGN ||
           token_type == Python3Parser::SUB_ASSIGN ||
           token_type == Python3Parser::MULT_ASSIGN ||
           token_type == Python3Parser::AT_ASSIGN ||
           token_type == Python3Parser::DIV_ASSIGN ||
           token_type == Python3Parser::MOD_ASSIGN ||
           token_type == Python3Parser::AND_ASSIGN ||
           token_type == Python3Parser::OR_ASSIGN ||
           token_type == Python3Parser::XOR_ASSIGN ||
           token_type == Python3Parser::LEFT_SHIFT_ASSIGN ||
           token_type == Python3Parser::RIGHT_SHIFT_ASSIGN ||
           token_type == Python3Parser::POWER_ASSIGN ||
           token_type == Python3Parser::IDIV_ASSIGN;
}

inline bool isArithOp(const Token *tok) {
    const auto token_type = tok->getType();
    return token_type == Python3Parser::ADD || token_type == Python3Parser::MINUS;
}

inline bool isTermOp(const Token *tok) {
    const auto token_type = tok->getType();
    return token_type == Python3Parser::STAR ||
           token_type == Python3Parser::DIV ||
           token_type == Python3Parser::IDIV ||
           token_type == Python3Parser::MOD;
}

inline bool isTestlistComp(const Token *tok) {
    const auto token_type = tok->getType();
    return token_type == Python3Parser::STRING ||
           token_type == Python3Parser::NUMBER ||
           token_type == Python3Parser::LAMBDA ||
           token_type == Python3Parser::NOT ||
           token_type == Python3Parser::NONE ||
           token_type == Python3Parser::TRUE ||
           token_type == Python3Parser::FALSE ||
           token_type == Python3Parser::AWAIT ||
           token_type == Python3Parser::ELLIPSIS ||
           token_type == Python3Parser::STAR ||
           token_type == Python3Parser::OPEN_PAREN ||
           token_type == Python3Parser::OPEN_BRACK ||
           token_type == Python3Parser::ADD ||
           token_type == Python3Parser::MINUS ||
           token_type == Python3Parser::NOT_OP ||
           token_type == Python3Parser::OPEN_BRACE;
}

inline bool isCompOp(PyLexer &lexer) {
    if (!lexer.curr || !lexer.next) {
        return false;
    }

    const auto token_type = lexer.curr->getType();
    const auto next_token_type = lexer.next->getType();

    return token_type == Python3Parser::LESS_THAN ||
           token_type == Python3Parser::GREATER_THAN ||
           token_type == Python3Parser::EQUALS ||
           token_type == Python3Parser::GT_EQ ||
           token_type == Python3Parser::LT_EQ ||
           token_type == Python3Parser::NOT_EQ_2 ||
           token_type == Python3Parser::IN ||
           (token_type == Python3Parser::NOT && next_token_type == Python3Parser::IN) ||
           token_type == Python3Parser::IS ||
           (token_type == Python3Parser::IS && next_token_type == Python3Parser::NOT);
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

struct Comprehension : public Node {
    explicit Comprehension(Node *target, Node *iter, std::vector<IfStmt *> ifs, bool is_async)
            : target(target), iter(iter), ifs(ifs), is_async(is_async) {}

    Node *target;
    Node *iter;
    std::vector<IfStmt *> ifs;
    bool is_async;
};

struct GeneratorExp : public Node {
    explicit GeneratorExp(Node *elt, std::vector<Node *> generators)
            : elt(elt), generators(generators) {}

    virtual std::vector<Node *> getChildren() const {
        auto temp = generators;
        temp.push_back(elt);
        return temp;
    }

    Node *elt;
    std::vector<Node *> generators;
};

struct Dict : public Node {
    explicit Dict(std::vector<Node *> keys, std::vector<Node *> values)
            : keys(keys), values(values) {}

    std::vector<Node *> keys;
    std::vector<Node *> values;
};

struct DictComp : public Node {
    explicit DictComp(Node *key, Node *value, std::vector<Node *> generators)
            : key(key), value(value), generators(generators) {}

    Node *key;
    Node *value;
    std::vector<Node *> generators;
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

struct SimpleStmt : public Node {
    explicit SimpleStmt(const std::vector<Node *> &small_stmts)
            : small_stmts(small_stmts) {}


    std::vector<Node *> small_stmts;
};

struct ForStmt : public Node {
    explicit ForStmt() {}
};

struct TryStmt : public Node {
};

struct ExprList : public Node {
    explicit ExprList(const std::vector<Node *> &expr_list)
            : expr_list(expr_list) {}

    virtual std::string str() const noexcept override {
        return "";
    }

    std::vector<Node *> expr_list;
};

struct Test : public Node {
    explicit Test();
};

struct ClassDef : public Node {
    explicit ClassDef() {}
};

struct Attribute : public Node {
    explicit Attribute(Node *value, Node *attr)
            : value(value), attr(attr) {}

    virtual std::vector<Node *> getChildren() const override {
        return {value, attr};
    }

    Node *value;
    Node *attr;
};


struct IfStmt : public Node {
    explicit IfStmt(Node *test, Node *body, Node *or_else)
            : test(test), body(body), or_else(or_else) {}

    virtual std::string str() const noexcept override {
        std::ostringstream if_str;
        if_str << "IfStmt(test=" << test->str() << ",body=" << body->str() << ",or_else=" << or_else->str() << ")";
        return if_str.str();
    }

    virtual std::vector<Node *> getChildren() const override {
        return {test, body, or_else};
    }

    Node *test;
    Node *body;
    Node *or_else;
};

struct Import : public Node {
    explicit Import(Aliases *aliases)
            : aliases(aliases) {}

    virtual std::vector<Node *> getChildren() const override {
        return {reinterpret_cast<Node *>(aliases)};
    }

    Aliases *aliases;
};

struct ImportFrom : public Node {
    explicit ImportFrom(Name *module, Aliases *aliases, int32_t level)
            : module(module), aliases(aliases), level(level) {}

    Name *module;
    Aliases *aliases;
    int32_t level;
};

struct TestList : public Node {
    explicit TestList(const std::vector<Node *> &nodes)
            : nodes(nodes) {}

    virtual std::string str() const noexcept override {
        std::ostringstream test_list_str;
        for (const auto node: nodes) {
            test_list_str << node->str();
        }
        return test_list_str.str();
    }

    std::vector<Node *> nodes;
};


struct Return : public Node {
    explicit Return(TestList *test_list)
            : test_list(test_list) {}


    virtual std::string str() const noexcept override {
        std::ostringstream return_str;
        return_str << "Return(expr=" << test_list->str() << ")";
        return return_str.str();
    }

    TestList *test_list;
};

struct Break : public Node {
    Break() {}
};

struct Continue : public Node {
    Continue() {}
};

struct Assign : public Node {
    explicit Assign(TestList *targets, Node *value)
            : targets(targets), value(value) {}

    TestList *targets;
    Node *value;
};

struct AugAssign : public Node {
    explicit AugAssign(Node *target, const int32_t op, Node *value)
            : target(target), op(op), value(value) {}

    Node *target;
    int32_t op;
    Node *value;
};


struct AnnAssign : public Node {
    explicit AnnAssign(Node *target, Node *annotation, Node *value)
            : target(target), annotation(annotation), value(value) {}

    Node *target;
    Node *annotation;
    Node *value;
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

struct StarredExpr : public Node {
    explicit StarredExpr(Node *expr)
            : expr(expr) {}

    std::string str() const noexcept override {
        std::ostringstream starred_expr_str;
        starred_expr_str << "StarredExpr(expr=" << expr->str() << ")";
        return starred_expr_str.str();
    }

    Node *expr;
};

struct WhileStmt : public Node {
    explicit WhileStmt(Node *test, Node *body, Node *or_else)
            : test(test), body(body), or_else(or_else) {}


    Node *test;
    Node *body;
    Node *or_else;
};


struct Raise : public Node {
    explicit Raise(Node *exception, Node *from)
            : exception(exception), from(from) {}

    Node *exception;
    Node *from;
};

struct Yield : public Node {
    explicit Yield(Node *target)
            : target(target) {}

    Node *target;
};

struct YieldFrom : public Node {
    explicit YieldFrom(Node *target)
            : target(target) {}

    Node *target;
};

struct Delete : public Node {
    explicit Delete(ExprList *targets)
            : targets(targets) {}

    std::string str() const noexcept override {
        std::ostringstream delete_str;
        delete_str << "Delete(target=" << targets->str() << ")";
        return delete_str.str();
    }

    ExprList *targets;
};

struct Const : public Node {
    explicit Const(const std::string &value, const int32_t type)
            : value(value), type(type) {}

    std::string str() const noexcept override {
        std::ostringstream const_str;
        const_str << "Const(value=" << value << ",type=" << tok_utils::tokTypeToStr[type] << ")";
        return const_str.str();
    }

    virtual std::vector<Node *> getChildren() const override {
        return {};
    }

    std::string value;
    int32_t type;
};

struct Arguments : public Node {
    explicit Arguments(const std::vector<Node *> &args, const std::vector<Node *> &keywords)
            : args(args), keywords(keywords) {}

    virtual std::vector<Node *> getChildren() const {
        // coalesce two vectors into the single one
        std::vector<Node *> temp;
        temp = args;
        temp.insert(temp.end(), keywords.begin(), keywords.end());

        return temp;
    }

    std::vector<Node *> args;
    std::vector<Node *> keywords;
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

struct Parameter : public Node {
    explicit Parameter(Name *name, Node *type, Node *default_val)
            : name(name), type(type), default_val(default_val) {}

    std::string str() const noexcept override {
        std::ostringstream arg_str;

        arg_str << "Parameter(name=" << name->str() << ",type=" << type->str() << ",default_val=" << default_val->str()
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

struct Parameters : public Node {

    struct ExtraParamData {
        Node *kwarg;
        Node *vararg;
        std::vector<Node *> pos_only_args;
        std::vector<Node *> kw_only_args;
        std::vector<Node *> kw_defaults;
        std::vector<Node *> defaults;
    };

    explicit Parameters(const std::vector<Node *> &params, const ExtraParamData &extra)
            : params(params), extra(extra) {}

    std::vector<Node *> params;
    ExtraParamData extra;
};

struct Keyword : public Node {
    explicit Keyword(const std::string &arg, Node *value)
            : arg(arg), value(value) {}

    std::string arg;
    Node *value;
};


struct Alias {
    explicit Alias(Name *name, Name *as)
            : name(name), as(as) {}

    Name *name;
    Name *as;
};

struct Aliases {
    explicit Aliases(const std::vector<Alias *> aliases)
            : aliases(aliases) {}

    std::vector<Alias *> aliases;
};

struct BinOp : public Node {
    explicit BinOp(Node *left, Node *right, int32_t op) : left(left), right(right), op(op) {}

    std::string str() const noexcept override {
        std::ostringstream bin_op_str;
        bin_op_str << "BinOp(left=" << left->str() << ",right=" << right->str() << ",op=" << tok_utils::tokTypeToStr[op]
                   << ")";
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
            : expr(expr), op(op) {}

    std::string str() const noexcept override {
        std::ostringstream unary_op_str;
        unary_op_str << "UnaryOp(expr=" << expr->str() << ",op=" << tok_utils::tokTypeToStr[op] << ")";
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
        bool_op_str << "BoolOp(left = " << left->str() << ",right=" << right->str() << ",op="
                    << tok_utils::tokTypeToStr[op]
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
        comparison_str << "Comparison(left=" << left->str() << ",right=" << right->str() << ",op="
                       << tok_utils::tokTypeToStr[op]
                       << ")";
        return comparison_str.str();
    }

    Node *left;
    Node *right;
    int32_t op;
};

struct FuncDef : public Node {
    explicit FuncDef(Name *name, Parameters *parameters, Node *body, Node *return_type) :
            name(name), parameters(parameters), body(body), return_type(return_type) {};

    std::string str() const noexcept override {
        std::ostringstream func_def_str;
        func_def_str << "FuncDef(name=" << name->str() << ",arguments="
                     << parameters->str() << ",body=" << body->str()
                     << ",return_type=" << return_type->str();

        return func_def_str.str();
    }

    Name *name;
    Parameters *parameters;
    Node *body;
    Node *return_type;
};

struct Lambda : public Node {
    explicit Lambda(Node *args, Node *body)
            : args(args), body(body) {}


    virtual std::vector<Node *> getChildren() const override {
        return {args, body};
    }

    Node *args;
    Node *body;
};

struct Call : public Node {
    explicit Call(Node *func, Arguments *arguments)
            : func(func), arguments(arguments) {}

    virtual std::vector<Node *> getChildren() const {
        return {func, arguments};
    }

    Node *func;
    Arguments *arguments;
};

struct Pass : public Node {
    Pass() {}
};

struct Global : public Node {
    explicit Global(const std::vector<Name *> &names)
            : names(names) {}

    std::vector<Name *> names;
};

struct Assert : public Node {
    explicit Assert(Node *test, Node *message)
            : test(test), message(message) {}

    Node *test;
    Node *message;
};

struct Nonlocal : public Node {
    explicit Nonlocal(const std::vector<Name *> &names)
            : names(names) {}

    std::vector<Name *> names;
};
