#pragma once

#include <iostream>
#include <vector>
#include <stack>
#include <optional>
#include <any>
#include <sstream>
#include <map>
#include <execinfo.h>

#include <numeric>
#include "antlr4-runtime.h"
#include "Python3Lexer.h"

#define ERR_TOK(expected_token_type) fprintf(stderr, "expected token %d", expected_token_type);
#define ERR_MSG(message) fprintf(stderr, message)
#define ERR_MSG_EXIT(message) ERR_MSG(message); exit(1)
// TODO(threadedstream): define cleanup exit macro
//#define ERR_MSG_CLEANUP_EXIT(message) ERR_MSG(message); destroyAst()

template<typename T>
using opt = std::optional<T>;


using namespace antlr4;

enum class AssignFlag : uint8_t {
    OP_ASSIGN = 0,
};

enum {
    NOT_IN = 1,
    LESS_THAN,
    GREATER_THAN,
    EQUALS,
    GT_EQ,
    LT_EQ,
    NOT_EQ_2,
    IN,
    IS,
    IS_NOT
};

struct Node;
struct FileInput;
struct Module;
struct SimpleStmt;
struct FuncDef;
struct Lambda;
struct List;
struct ListComp;
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
struct SubscriptList;
struct ImportFrom;
struct Raise;
struct Subscript;
struct Break;
struct Alias;
struct Aliases;
struct Continue;
struct Attribute;
struct TestList;
struct Return;
struct Const;
struct Index;
struct Slice;
struct Argument;
struct Arguments;
struct DictComp;
struct Parameter;
struct Parameters;
struct SetComp;
struct Dict;
struct ExceptHandler;
struct Set;
struct Global;
struct Assert;
struct Nonlocal;
struct IfStmt;
struct WhileStmt;
struct ForStmt;
struct AsyncForStmt;
struct TryStmt;
struct WithStmt;
struct AsyncWithStmt;
struct WithItem;

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

Node *parseSingleInput(PyLexer &lexer);

Node *parseFileInput(PyLexer &lexer);

Node *parseEvalInput(PyLexer &lexer);

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

Node *parseAsyncForStmt(PyLexer &lexer);

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

FuncDef *parseFuncDef(PyLexer &lexer, std::vector<Node *> &&decorator_list);

AsyncFuncDef *parseAsyncFuncDef(PyLexer &lexer, std::vector<Node *> &decorator_list);

AsyncWithStmt *parseAsyncWithStmt(PyLexer &lexer);

ClassDef *parseClassDef(PyLexer &lexer, std::vector<Node *> &&decorator_list);

Node *parseSuite(PyLexer &lexer);

Subscript *parseSubscriptList(PyLexer &lexer);

Subscript *parseSubscript(PyLexer &lexer);

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

WithItem *parseWithItem(PyLexer &lexer);

WithStmt *parseWithStmt(PyLexer &lexer);

ForStmt *parseForStmt(PyLexer &lexer);

WithStmt *parseWithStmt(PyLexer &lexer);

TryStmt *parseTryStmt(PyLexer &lexer);

ExceptHandler *parseExceptClause(PyLexer &lexer);

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

Node *parseDottedName(PyLexer &lexer, bool as_attr = false);

Node *parseDecorated(PyLexer &lexer);

Node *parseDecorator(PyLexer &lexer);

std::vector<Node *> parseDecorators(PyLexer &lexer);

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

void destroyNode(Node *root);

namespace tok_utils {
    static const char *comparisonOpToStr(const int32_t op) {
        switch (op) {
            case NOT_IN:
                return "NotIn";
            case LESS_THAN:
                return "LessThan";
            case GREATER_THAN:
                return "GreaterThan";
            case EQUALS:
                return "Equals";
            case GT_EQ:
                return "GtEq";
            case LT_EQ:
                return "LtEQ";
            case NOT_EQ_2:
                return "NotEq2";
            case IN:
                return "In";
            case IS:
                return "Is";
            case IS_NOT:
                return "IsNot";
            default:
                return "Unknown";
        }
    }

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
    explicit PyLexer(const char *path) {
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
            fprintf(stderr, "expected %s at line %ld", tok_utils::tokTypeToStr[token_type].c_str(), curr->getLine());
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

struct Position {
    size_t line_start;
    size_t line_end;
    size_t col_start_idx;
    size_t col_end_idx;
} __attribute__((aligned(16)));


inline Position getTokPos(const Token *tok) {
    Position pos = {};
    pos.line_start = tok->getLine();
    pos.col_start_idx = tok->getStartIndex();
    pos.col_end_idx = tok->getStopIndex();

    return pos;
}

static inline bool isStmt(const Token *tok) {
    const auto current_token_type = tok->getType();
    return current_token_type == Python3Parser::STRING ||
           current_token_type == Python3Parser::NUMBER ||
           current_token_type == Python3Parser::DEF ||
           current_token_type == Python3Parser::RETURN ||
           current_token_type == Python3Parser::RAISE ||
           current_token_type == Python3Parser::FROM ||
           current_token_type == Python3Parser::IMPORT ||
           current_token_type == Python3Parser::GLOBAL ||
           current_token_type == Python3Parser::NONLOCAL ||
           current_token_type == Python3Parser::ASSERT ||
           current_token_type == Python3Parser::IF ||
           current_token_type == Python3Parser::WHILE ||
           current_token_type == Python3Parser::FOR ||
           current_token_type == Python3Parser::TRY ||
           current_token_type == Python3Parser::WITH ||
           current_token_type == Python3Parser::LAMBDA ||
           current_token_type == Python3Parser::NOT ||
           current_token_type == Python3Parser::NONE ||
           current_token_type == Python3Parser::TRUE ||
           current_token_type == Python3Parser::FALSE ||
           current_token_type == Python3Parser::CLASS ||
           current_token_type == Python3Parser::YIELD ||
           current_token_type == Python3Parser::DEL ||
           current_token_type == Python3Parser::PASS ||
           current_token_type == Python3Parser::CONTINUE ||
           current_token_type == Python3Parser::BREAK ||
           current_token_type == Python3Parser::ASYNC ||
           current_token_type == Python3Parser::AWAIT ||
           current_token_type == Python3Parser::NEWLINE ||
           current_token_type == Python3Parser::NAME ||
           current_token_type == Python3Parser::ELLIPSIS ||
           current_token_type == Python3Parser::STAR ||
           current_token_type == Python3Parser::OPEN_PAREN ||
           current_token_type == Python3Parser::OPEN_BRACK ||
           current_token_type == Python3Parser::ADD ||
           current_token_type == Python3Parser::MINUS ||
           current_token_type == Python3Parser::NOT_OP ||
           current_token_type == Python3Parser::OPEN_BRACE ||
           current_token_type == Python3Parser::AT;
}

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

inline bool isTest(const Token *tok) {
    const auto token_type = tok->getType();
    return token_type == Python3Parser::STRING ||
           token_type == Python3Parser::NUMBER ||
           token_type == Python3Parser::LAMBDA ||
           token_type == Python3Parser::NOT ||
           token_type == Python3Parser::NONE ||
           token_type == Python3Parser::TRUE ||
           token_type == Python3Parser::FALSE ||
           token_type == Python3Parser::AWAIT ||
           token_type == Python3Parser::NAME ||
           token_type == Python3Parser::ELLIPSIS ||
           token_type == Python3Parser::OPEN_PAREN ||
           token_type == Python3Parser::OPEN_BRACK ||
           token_type == Python3Parser::ADD ||
           token_type == Python3Parser::MINUS ||
           token_type == Python3Parser::NOT_OP ||
           token_type == Python3Parser::OPEN_BRACE;
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
           token_type == Python3Parser::NAME ||
           token_type == Python3Parser::ELLIPSIS ||
           token_type == Python3Parser::STAR ||
           token_type == Python3Parser::OPEN_PAREN ||
           token_type == Python3Parser::OPEN_BRACK ||
           token_type == Python3Parser::ADD ||
           token_type == Python3Parser::MINUS ||
           token_type == Python3Parser::NOT_OP ||
           token_type == Python3Parser::OPEN_BRACE;
}

inline bool isCompOp(PyLexer &lexer, bool &eat_twice, int32_t &op_type) {
    if (!lexer.curr || !lexer.next) {
        return false;
    }

    const auto token_type = lexer.curr->getType();
    const auto next_token_type = lexer.next->getType();

    if (token_type == Python3Parser::NOT && next_token_type == Python3Parser::IN
        && (token_type == Python3Parser::IS && next_token_type == Python3Parser::NOT) && eat_twice) {
        eat_twice = true;
    } else if (eat_twice) {
        eat_twice = false;
    }

    return (op_type = (LESS_THAN * (token_type == Python3Parser::LESS_THAN)) ||
                      (GREATER_THAN * (token_type == Python3Parser::GREATER_THAN)) ||
                      (EQUALS * (token_type == Python3Parser::EQUALS)) ||
                      (GT_EQ * (token_type == Python3Parser::GT_EQ)) ||
                      (LT_EQ * (token_type == Python3Parser::LT_EQ)) ||
                      (NOT_EQ_2 * (token_type == Python3Parser::NOT_EQ_2)) ||
                      (IN * (token_type == Python3Parser::IN)) ||
                      (NOT_IN * (token_type == Python3Parser::NOT && next_token_type == Python3Parser::IN)) ||
                      (IS * token_type == Python3Parser::IS) ||
                      (IS_NOT * (token_type == Python3Parser::IS && next_token_type == Python3Parser::NOT)));

}

struct Node {
    // TODO(threadedstream): fill in the rest

    virtual ~Node() {};

    virtual std::string str() const noexcept {
        return "";
    }

    virtual std::vector<Node *> getChildren() { return {}; }

    // n - child to add to the parent node
    // next_arg - needed for functions (likely to be removed)
    virtual void addChild(Node *n, const bool next_arg = false) noexcept {}

    Position pos_info;
};

struct FileInput : public Node {
    explicit FileInput(const std::vector<Node *> statements)
            : statements(statements) {}

    std::vector<Node *> statements;
};

struct Module : public Node {
    Module(const std::string &doc, Node *expr) : expr(expr), doc(doc) {}

    std::string str() const noexcept override {
        std::ostringstream module_str;
        module_str << "Module(doc=" << (doc != "" ? doc : "None") << ",expr=" << expr->str() << ")";
        return module_str.str();
    }

    virtual std::vector<Node *> getChildren() {
        return {expr};
    }

    Node *expr;
    std::string doc;
};

struct Comprehension : public Node {
    explicit Comprehension(Node *target, Node *iter, std::vector<Node *> ifs, bool is_async)
            : target(target), iter(iter), ifs(ifs), is_async(is_async) {}


    virtual std::vector<Node *> getChildren() {
        auto temp = ifs;
        temp.push_back(target);
        temp.push_back(iter);

        return temp;
    }

    Node *target;
    Node *iter;
    std::vector<Node *> ifs;
    bool is_async;
};

struct GeneratorExp : public Node {
    explicit GeneratorExp(Node *elt, std::vector<Node *> generators)
            : elt(elt), generators(generators) {}

    virtual std::vector<Node *> getChildren() {
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

    virtual std::vector<Node *> getChildren() override {
        auto temp = keys;
        temp.insert(temp.end(), values.begin(), values.end());
        return temp;
    }

    std::vector<Node *> keys;
    std::vector<Node *> values;
};

struct DictComp : public Node {
    explicit DictComp(Node *key, Node *value, const std::vector<Node *> &generators)
            : key(key), value(value), generators(generators) {}


    virtual std::vector<Node *> getChildren() override {
        auto temp = generators;
        temp.push_back(key);
        temp.push_back(value);
        return temp;
    }

    Node *key;
    Node *value;
    std::vector<Node *> generators;
};

struct Set : public Node {
    explicit Set(const std::vector<Node *> &elements)
            : elements(elements) {}

    virtual std::vector<Node *> getChildren() override {
        auto temp = elements;
        return temp;
    }

    std::vector<Node *> elements;
};

struct List : public Node {
    explicit List(const std::vector<Node *> &elements)
            : elements(elements) {}

    virtual std::vector<Node *> getChildren() override {
        auto temp = elements;
        return temp;
    }

    std::vector<Node *> elements;
};

struct ListComp : public Node {
    explicit ListComp(Node *value, const std::vector<Node *> &generators)
            : value(value), generators(generators) {}


    virtual std::vector<Node *> getChildren() override {
        auto temp = generators;
        generators.push_back(value);
        return temp;
    }

    Node *value;
    std::vector<Node *> generators;
};

struct SetComp : public Node {
    explicit SetComp(Node *value, const std::vector<Node *> &generators)
            : value(value), generators(generators) {}

    virtual std::vector<Node *> getChildren() override {
        auto temp = generators;
        generators.push_back(value);
        return temp;
    }

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

    virtual std::vector<Node *> getChildren() {
        auto temp = nodes;
        return temp;
    }

    std::vector<Node *> nodes;
};

struct SimpleStmt : public Node {
    explicit SimpleStmt(const std::vector<Node *> &small_stmts)
            : small_stmts(small_stmts) {}


    std::vector<Node *> small_stmts;
};

struct ForStmt : public Node {
    explicit ForStmt(Node *target, Node *iter, Node *body, Node *or_else)
            : target(target), iter(iter), body(body), or_else(or_else) {}

    virtual std::vector<Node *> getChildren() override {
        return {target, iter, body, or_else};
    }

    Node *target;
    Node *iter;
    Node *body;
    Node *or_else;
};

struct AsyncForStmt : public Node {
    explicit AsyncForStmt(Node *target, Node *iter, Node *body, Node *or_else)
            : target(target), iter(iter), body(body), or_else(or_else) {}

    explicit AsyncForStmt(ForStmt *for_stmt, bool destroy_for_stmt) :
            target(for_stmt->target), iter(for_stmt->iter), body(for_stmt->body), or_else(for_stmt->or_else) {
        if (destroy_for_stmt) {
            delete for_stmt;
            for_stmt = nullptr;
        }
    }

    virtual std::vector<Node *> getChildren() override {
        return {target, iter, body, or_else};
    }


    Node *target;
    Node *iter;
    Node *body;
    Node *or_else;
};

struct WithStmt : public Node {
    explicit WithStmt(const std::vector<Node *> items, Node *body, Node *type_comment)
            : body(body), type_comment(type_comment), items(items) {}

    virtual std::vector<Node *> getChildren() override {
        // coalesce two vectors into the single one
        auto temp = items;
        temp.push_back(body);
        temp.push_back(type_comment);
        return temp;
    }

    Node *body;
    Node *type_comment;
    std::vector<Node *> items;
};

struct AsyncWithStmt : public Node {
    explicit AsyncWithStmt(const std::vector<Node *> items, Node *body, Node *type_comment)
            : body(body), type_comment(type_comment), items(items) {}

    explicit AsyncWithStmt(WithStmt *with_stmt, bool destroy_with_stmt)
            : body(with_stmt->body), type_comment(with_stmt->type_comment), items(with_stmt->items) {
        if (destroy_with_stmt) {
            delete with_stmt;
            with_stmt = nullptr;
        }
    }

    virtual std::vector<Node *> getChildren() override {
        // coalesce two vectors into the single one
        auto temp = items;
        temp.push_back(body);
        temp.push_back(type_comment);
        return temp;
    }


    Node *body;
    Node *type_comment;
    std::vector<Node *> items;
};

struct WithItem : public Node {
    explicit WithItem(Node *context_expr, Node *optional_vars)
            : context_expr(context_expr), optional_vars(optional_vars) {}

    virtual std::vector<Node *> getChildren() override {
        return {context_expr, optional_vars};
    }

    Node *context_expr;
    Node *optional_vars;
};

struct TryStmt : public Node {
    explicit TryStmt(Node *body, std::vector<Node *> handlers, Node *or_else, Node *final_body)
            : body(body), or_else(or_else), final_body(final_body), handlers(handlers) {}


    virtual std::vector<Node *> getChildren() override {
        auto temp = handlers;
        temp.push_back(body);
        temp.push_back(or_else);
        temp.push_back(final_body);
        return temp;
    }

    Node *body;
    Node *or_else;
    Node *final_body;
    std::vector<Node *> handlers;
};

struct ExceptHandler : public Node {
    explicit ExceptHandler(Node *type, const std::string &name, Node *body)
            : type(type), body(body), name(name) {}

    virtual std::vector<Node *> getChildren() override {
        return {type, body};
    }

    Node *type;
    Node *body;
    std::string name;
};

struct ExprList : public Node {
    explicit ExprList(const std::vector<Node *> &expr_list)
            : expr_list(expr_list) {}

    virtual std::string str() const noexcept override {
        return "";
    }

    virtual std::vector<Node *> getChildren() override {
        auto temp = expr_list;
        return temp;
    }

    std::vector<Node *> expr_list;
};

struct ClassDef : public Node {
    explicit ClassDef(const std::string &name, Arguments *arguments,
                      Node *body, std::vector<Node *> decorator_list)
            : body(body), arguments(arguments), decorator_list(decorator_list), name(name) {}

    virtual std::vector<Node *> getChildren() override {
        auto temp = decorator_list;
        temp.push_back(body);
        temp.push_back(reinterpret_cast<Node *>(arguments));
        return temp;
    }

    Node *body;
    Arguments *arguments;
    std::vector<Node *> decorator_list;
    std::string name;
};

struct Attribute : public Node {
    explicit Attribute(Node *value, Node *attr)
            : value(value), attr(attr) {}

    virtual std::vector<Node *> getChildren() override {
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

    virtual std::vector<Node *> getChildren() override {
        return {test, body, or_else};
    }

    Node *test;
    Node *body;
    Node *or_else;
};

struct Import : public Node {
    explicit Import(Aliases *aliases)
            : aliases(aliases) {}

    virtual std::vector<Node *> getChildren() override {
        return {reinterpret_cast<Node *>(aliases)};
    }

    Aliases *aliases;
};

struct ImportFrom : public Node {
    explicit ImportFrom(Node *module, Aliases *aliases, int32_t level)
            : module(module), aliases(aliases), level(level) {}

    virtual std::vector<Node *> getChildren() override {
        return {module, reinterpret_cast<Node *>(aliases)};
    }

    Node *module;
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

    virtual std::vector<Node *> getChildren() override {
        auto temp = nodes;
        return temp;
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

    virtual std::vector<Node *> getChildren() override {
        return {test_list};
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

    virtual std::vector<Node *> getChildren() override {
        return {targets, value};
    }

    TestList *targets;
    Node *value;
};

struct AugAssign : public Node {
    explicit AugAssign(Node *target, const int32_t op, Node *value)
            : target(target), value(value), op(op) {}


    virtual std::vector<Node *> getChildren() override {
        return {target, value};
    }

    Node *target;
    Node *value;
    int32_t op;
};


struct AnnAssign : public Node {
    explicit AnnAssign(Node *target, Node *annotation, Node *value)
            : target(target), annotation(annotation), value(value) {}

    virtual std::vector<Node *> getChildren() override {
        return {target, annotation, value};
    }

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

struct Index : public Node {
    explicit Index(Node *value)
            : value(value) {}

    Node *value;
};

struct Slice : public Node {
    explicit Slice(Node *lower, Node *upper, Node *step)
            : lower(lower), upper(upper), step(step) {}

    Node *lower;
    Node *upper;
    Node *step;
};

struct Subscript : public Node {
    explicit Subscript(Node *value, Node *slice)
            : value(value), slice(slice) {}

    Node *value;
    Node *slice;
};

struct ExtSlice : public Node {
    explicit ExtSlice(const std::vector<Node *> &dims)
            : dims(dims) {}

    std::vector<Node *> dims;
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

    virtual std::vector<Node *> getChildren() override {
        return {};
    }

    std::string value;
    int32_t type;
};

struct Arguments : public Node {
    explicit Arguments(const std::vector<Node *> &args, const std::vector<Node *> &keywords)
            : args(args), keywords(keywords) {}

    virtual std::vector<Node *> getChildren() {
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

    virtual std::vector<Node *> getChildren() override {
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

    virtual std::vector<Node *> getChildren() override {
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
    explicit Alias(Node *name, Name *as)
            : name(name), as(as) {}

    Node *name;
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

    virtual std::vector<Node *> getChildren() override {
        return {left, right};
    }

    Node *left;
    Node *right;
    int32_t op;
};

struct SubscriptList : public Node {
    explicit SubscriptList(Node *value, const std::vector<Node *> &subscripts)
            : value(value), subscripts(subscripts) {}

    Node *value;
    std::vector<Node *> subscripts;
};

struct UnaryOp : public Node {
    explicit UnaryOp(const int32_t op, Node *expr)
            : expr(expr), op(op) {}

    std::string str() const noexcept override {
        std::ostringstream unary_op_str;
        unary_op_str << "UnaryOp(expr=" << expr->str() << ",op=" << tok_utils::tokTypeToStr[op] << ")";
        return unary_op_str.str();
    }

    virtual std::vector<Node *> getChildren() override {
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

    virtual std::vector<Node *> getChildren() override {
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
                       << tok_utils::comparisonOpToStr(op)
                       << ")";
        return comparison_str.str();
    }

    Node *left;
    Node *right;
    int32_t op;
};

struct FuncDef : public Node {
    explicit FuncDef(const std::string &name, Parameters *parameters, Node *body, Node *return_type,
                     std::vector<Node *> decorator_list) :
            name(name), parameters(parameters), body(body), return_type(return_type), decorator_list(decorator_list) {};

    std::string str() const noexcept override {
        std::ostringstream func_def_str;
        func_def_str << "FuncDef(name=" << name << ",arguments="
                     << parameters->str() << ",body=" << body->str()
                     << ",return_type=" << return_type->str();

        return func_def_str.str();
    }

    virtual std::vector<Node *> getChildren() {
        auto temp = decorator_list;
        temp.push_back(parameters);
        temp.push_back(body);
        temp.push_back(return_type);
        return temp;
    }

    std::string name;
    Parameters *parameters;
    Node *body;
    Node *return_type;
    std::vector<Node *> decorator_list;
};

struct AsyncFuncDef : public Node {
    explicit AsyncFuncDef(const std::string &name, Parameters *parameters, Node *body, Node *return_type,
                          const std::vector<Node *> &decorator_list,
                          Node *type_comment)
            : name(name), parameters(parameters), body(body), return_type(return_type), type_comment(type_comment),
              decorator_list(decorator_list) {}

    explicit AsyncFuncDef(FuncDef *func_def, bool destroy_func_def)
            : name(func_def->name), parameters(func_def->parameters), body(func_def->body),
              return_type(func_def->return_type), decorator_list(func_def->decorator_list) {
        if (destroy_func_def) {
            delete func_def;
            func_def = nullptr;
        }
    }

    virtual std::vector<Node *> getChildren() {
        auto temp = decorator_list;
        temp.push_back(parameters);
        temp.push_back(body);
        temp.push_back(return_type);
        temp.push_back(type_comment);
        return temp;
    }

    std::string name;
    Node *parameters;
    Node *body;
    Node *return_type;
    Node *type_comment;
    std::vector<Node *> decorator_list;
};

struct Lambda : public Node {
    explicit Lambda(Node *args, Node *body)
            : args(args), body(body) {}


    virtual std::vector<Node *> getChildren() override {
        return {args, body};
    }

    Node *args;
    Node *body;
};

struct Call : public Node {
    explicit Call(Node *func, Arguments *arguments)
            : func(func), arguments(arguments) {}

    virtual std::vector<Node *> getChildren() {
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
