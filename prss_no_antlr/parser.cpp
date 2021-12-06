#include "parser.hpp"


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

Delete *parseDelStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::DEL);

    auto node = new Delete(nullptr);
    node->targets = parseExprList(lexer);

    return node;
}


Node *parseSimpleStmt(PyLexer &lexer) {
    auto simple_stmt = new SimpleStmt({});
    const auto node = parseSmallStmt(lexer);


    while (lexer.curr->getType() == Python3Lexer::SEMI_COLON) {
        lexer.consume(Python3Lexer::SEMI_COLON);

        const auto node = parseSmallStmt(lexer);
        simple_stmt->small_stmts.push_back(node);
    }

    lexer.consume(Python3Lexer::NEWLINE);
}

Node *parseSmallStmt(PyLexer &lexer) {
    switch (lexer.curr->getType()) {
        case Python3Lexer::ASSERT:
            return parseAssertStmt(lexer);
        case Python3Lexer::NONLOCAL:
            return parseNonlocalStmt(lexer);
        case Python3Lexer::GLOBAL:
            return parseGlobalStmt(lexer);
        case Python3Lexer::IMPORT:
            return parseImportStmt(lexer);
        case Python3Lexer::PASS:
            return parsePassStmt(lexer);
        case Python3Lexer::DEL:
            return parseDelStmt(lexer);
        case Python3Lexer::BREAK:
        case Python3Lexer::CONTINUE:
        case Python3Lexer::RETURN:
        case Python3Lexer::RAISE:
        case Python3Lexer::YIELD:
            return parseFlowStmt(lexer);
        default:
            return parseExprStmt(lexer);
    }
}

Node *parseFlowStmt(PyLexer &lexer) {
    switch (lexer.curr->getType()) {
        case Python3Lexer::BREAK:
            return parseBreakStmt(lexer);
        case Python3Lexer::CONTINUE:
            return parseContinueStmt(lexer);
        case Python3Lexer::RETURN:
            return parseReturnStmt(lexer);
        case Python3Lexer::RAISE:
            return parseRaiseStmt(lexer);
        case Python3Lexer::YIELD:
            return parseYieldStmt(lexer);
        default:
            // less likely to get here
            return nullptr;
    }
}


Pass *parsePassStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::PASS);
    auto pass = new Pass();
    return pass;
}

ExprList *parseExprList(PyLexer &lexer) {
    auto node = new ExprList({});

    if (lexer.curr->getType() == Python3Lexer::STAR) {
        node->expr_list.push_back(parseStarExpr(lexer));
    } else {
        node->expr_list.push_back(parseExpr(lexer));
    }

    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        if (lexer.curr->getType() == Python3Lexer::STAR) {
            node->expr_list.push_back(parseStarExpr(lexer));
        } else {
            node->expr_list.push_back(parseExpr(lexer));
        }
    }

    return node;
}

Node *parseSuite(PyLexer &lexer) {
    return nullptr;
}

Break *parseBreakStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::BREAK);
    auto node = new Break();
    return node;
}

Continue *parseContinueStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::CONTINUE);
    auto node = new Continue();
    return node;
}

TestList *parseTestlist(PyLexer &lexer) {
    auto test_list = new TestList({});

    auto node = parseTest(lexer);
    test_list->nodes.push_back(node);

    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        lexer.consume(Python3Lexer::COMMA);

        const auto node = parseTest(lexer);
        test_list->nodes.push_back(node);
    }

    return test_list;
}

Return *parseReturnStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::RETURN);
    auto node = new Return(nullptr);

    node->test_list = parseTestlist(lexer);

    return node;
}

Raise *parseRaiseStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::RAISE);

    auto raise = new Raise(nullptr, nullptr);

    raise->exception = parseTest(lexer);

    if (lexer.curr->getType() == Python3Lexer::FROM) {
        lexer.consume(Python3Lexer::FROM);

        raise->from = parseTest(lexer);
    }

    return raise;
}

Node *parseYieldStmt(PyLexer &lexer) {
     return parseYieldExpr(lexer);
}

Node *parseYieldExpr(PyLexer &lexer) {
    lexer.consume(Python3Lexer::YIELD);

    if (lexer.curr->getType() == Python3Lexer::FROM) {
        lexer.consume(Python3Lexer::FROM);

        auto yield_from = new YieldFrom(nullptr);
        yield_from->target = parseTest(lexer);
        return yield_from;
    }

    auto yield_stmt = new Yield(nullptr);

    yield_stmt->target = parseTestlist(lexer);

    return yield_stmt;
}

Node *parseYieldArg(PyLexer &lexer) {
    if (lexer.curr->getType() == Python3Lexer::FROM) {
        lexer.consume(Python3Lexer::FROM);
        auto yield_from = new YieldFrom(nullptr);
        yield_from->target = parseTest(lexer);
        return yield_from;
    }

    return parseTestlist(lexer);
}

TestList *parseTestList(PyLexer &lexer) {
    auto test_list = new TestList({});

    const auto node = parseTest(lexer);
    test_list->nodes.push_back(node);

    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        lexer.consume(Python3Lexer::COMMA);
        test_list->nodes.push_back(parseTest(lexer));
    }

    return test_list;
}

Node *parseSimpleStmt(PyLexer &lexer) {
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
        }
            break;
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

StarredExpr *parseStarExpr(PyLexer &lexer) {
    lexer.consume(Python3Lexer::STAR);

    auto node = new StarredExpr(nullptr);
    node->expr = parseExpr(lexer);

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



