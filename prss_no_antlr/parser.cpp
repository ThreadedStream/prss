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

    simple_stmt->small_stmts.push_back(node);
    while (lexer.curr->getType() == Python3Lexer::SEMI_COLON) {
        lexer.consume(Python3Lexer::SEMI_COLON);

        // handle a terminal semicolon
        if (lexer.curr->getType() == Python3Lexer::EOF ||
            lexer.curr->getType() == Python3Lexer::NEWLINE) {
            break;
        }
        const auto node = parseSmallStmt(lexer);
        simple_stmt->small_stmts.push_back(node);
    }

    if (lexer.curr->getType() == Python3Lexer::NEWLINE) {
        lexer.consume(Python3Lexer::NEWLINE);
    }

    return simple_stmt;
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

Assert *parseAssertStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::ASSERT);

    auto assert = new Assert(nullptr, nullptr);

    assert->test = parseTest(lexer);

    if (lexer.curr->getType() == Python3Lexer::COMMA) {
        lexer.consume(Python3Lexer::COMMA);
        assert->message = parseTest(lexer);
    }

    return assert;
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


ImportFrom *parseImportFrom(PyLexer &lexer) {
    lexer.consume(Python3Lexer::FROM);
    auto import_from = new ImportFrom(nullptr, nullptr, 0);
    int32_t level;

    // TODO(threadedstream): handle case with ellipsis
    while (lexer.curr->getType() == Python3Lexer::DOT) {
        lexer.consume(Python3Lexer::DOT);
        level++;
    }

    if (level == 0 && !(lexer.curr->getType() == Python3Lexer::NAME)) {
        // TODO(threadedstream): adhoc method to report an error and terminate application
        // call to destroyAst() would be great
        ERR_TOK(Python3Lexer::NAME);
        exit(1);
    }

    import_from->level = level;
    if (lexer.curr->getType() == Python3Lexer::NAME) {
        const auto name = parseDottedName(lexer);
        import_from->module = name;
    }

    lexer.consume(Python3Lexer::IMPORT);

    // may look really dumb
    switch (lexer.curr->getType()) {
        case Python3Lexer::STAR: {
            const auto name = new Name("*");
            import_from->aliases->aliases.push_back(new Alias(name, nullptr));
        }
            break;
        case Python3Lexer::OPEN_PAREN: {
            lexer.consume(Python3Lexer::OPEN_PAREN);
            import_from->aliases = parseImportAsNames(lexer);
            lexer.consume(Python3Lexer::CLOSE_PAREN);
        }
            break;
        case Python3Lexer::NAME:
            import_from->aliases = parseImportAsNames(lexer);
            break;
        default:
        ERR_MSG_EXIT("expected STAR, OPEN_PAREN or NAME\n");
    }

    return import_from;
}

Name *parseDottedName(PyLexer &lexer) {
    std::string name;
    const auto current_token = lexer.curr;
    lexer.consume(Python3Lexer::NAME);

    name += current_token->getText();

    while (lexer.curr->getType() == Python3Lexer::DOT) {
        lexer.consume(Python3Lexer::DOT);
        const auto current_token = lexer.curr;
        lexer.consume(Python3Lexer::NAME);
        name += current_token->getText();
    }

    auto dotted_name = new Name(name);

    return dotted_name;
}

Alias *parseDottedAsName(PyLexer &lexer) {
    const auto dotted_name = parseDottedName(lexer);
    auto alias = new Alias(dotted_name, nullptr);
    if (lexer.curr->getType() == Python3Lexer::AS) {
        lexer.consume(Python3Lexer::AS);
        alias->as = new Name(lexer.curr->getText());
    }

    return alias;
}

Aliases *parseDottedAsNames(PyLexer &lexer) {
    const auto dotted_as_name = parseDottedAsName(lexer);
    auto aliases = new Aliases({});
    aliases->aliases.push_back(dotted_as_name);

    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        lexer.consume(Python3Lexer::COMMA);
        const auto dotted_as_name = parseDottedAsName(lexer);
        aliases->aliases.push_back(dotted_as_name);
    }

    return aliases;
}

IfStmt *parseIfStmt(PyLexer &lexer, int32_t depth) {
    if (depth == 0) {
        lexer.consume(Python3Lexer::IF);
    }
    auto if_stmt = new IfStmt(nullptr, nullptr, nullptr);

    if_stmt->test = parseTest(lexer);
    lexer.consume(Python3Lexer::COLON);

    if_stmt->body = parseSuite(lexer);

    while (lexer.curr->getType() == Python3Lexer::ELIF) {
        lexer.consume(Python3Lexer::ELIF);
        if_stmt->or_else = parseIfStmt(lexer, depth + 1);
    }

    if (lexer.curr->getType() == Python3Lexer::ELSE) {
        lexer.consume(Python3Lexer::ELSE);
        lexer.consume(Python3Lexer::COLON);

        if_stmt->or_else = parseSuite(lexer);
    }

    return if_stmt;
}

WhileStmt *parseWhileStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::WHILE);
    auto while_stmt = new WhileStmt(nullptr, nullptr, nullptr);
    while_stmt->test = parseTest(lexer);
    lexer.consume(Python3Lexer::COLON);
    while_stmt->body = parseSuite(lexer);
    if (lexer.curr->getType() == Python3Lexer::ELSE) {
        lexer.consume(Python3Lexer::ELSE);
        lexer.consume(Python3Lexer::COLON);
        while_stmt->or_else = parseSuite(lexer);
    }

    return while_stmt;
}

Alias *parseImportAsName(PyLexer &lexer) {
    auto alias = new Alias(nullptr, nullptr);

    auto name = new Name(lexer.curr->getText());

    lexer.consume(Python3Lexer::NAME);
    lexer.consume(Python3Lexer::AS);

    auto as = new Name(lexer.curr->getText());

    lexer.consume(Python3Lexer::NAME);

    alias->name = name;
    alias->as = as;

    return alias;
}

Aliases *parseImportAsNames(PyLexer &lexer) {
    auto aliases = new Aliases({});
    const auto alias = parseImportAsName(lexer);

    aliases->aliases.push_back(alias);

    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        lexer.consume(Python3Lexer::COMMA);
        const auto alias = parseImportAsName(lexer);
        aliases->aliases.push_back(alias);
    }

    return aliases;
}

Node *parseStmt(PyLexer &lexer) {
    switch (lexer.curr->getType()) {
        case Python3Lexer::IF:
        case Python3Lexer::WHILE:
        case Python3Lexer::FOR:
        case Python3Lexer::TRY:
        case Python3Lexer::WITH:
        case Python3Lexer::DEF:
        case Python3Lexer::CLASS:
        case Python3Lexer::AT:
        case Python3Lexer::ASYNC:
            return parseCompoundStmt(lexer);
        default:
            return parseSimpleStmt(lexer);
    }
}

Node *parseCompIf(PyLexer &lexer) {
    lexer.consume(Python3Lexer::IF);

    const auto test_no_cond = parseTestNoCond(lexer);

    switch (lexer.curr->getType()) {
        case Python3Lexer::IF:
        case Python3Lexer::ASYNC:
        case Python3Lexer::FOR:
            const auto comp_iter = parseCompIter(lexer);
    }

    return nullptr;
}


Node *parseCompIter(PyLexer &lexer) {
    switch (lexer.curr->getType()) {
        case Python3Lexer::IF:
            return parseCompIf(lexer);
        case Python3Lexer::ASYNC
        case Python3Lexer::FOR:
            return parseCompFor(lexer);
        default:
        ERR_MSG_EXIT("Expected IF, ASYNC or FOR");
    }
}

Node *parseCompoundStmt(PyLexer &lexer) {
    switch (lexer.curr->getType()) {
        case Python3Lexer::IF:
            return parseIfStmt(lexer, 0);
        case Python3Lexer::WHILE:
            return parseWhileStmt(lexer);
        case Python3Lexer::FOR:
            return parseForStmt(lexer);
        case Python3Lexer::TRY:
            return parseTryStmt(lexer);
        case Python3Lexer::DEF:
            return parseFuncDef(lexer);
        case Python3Lexer::CLASS:
            return parseClassDef(lexer);
        case Python3Lexer::AT:
            return parseDecorated(lexer);
        case Python3Lexer::ASYNC:
            return parseAsyncStmt(lexer);
        default:
        ERR_MSG_EXIT("expected IF, WHILE, FOR, TRY, DEF, CLASS, AT or ASYNC");
    }
}

ClassDef *parseClassDef(PyLexer &lexer) {
    return nullptr;
}

Node *parseSuite(PyLexer &lexer) {
    if (lexer.curr->getType() == Python3Lexer::NEWLINE) {
        lexer.consume(Python3Lexer::NEWLINE);
        lexer.consume(Python3Parser::INDENT);
        const auto stmt = parseStmt(lexer);
        lexer.consume(Python3Parser::DEDENT);
        return stmt;
    } else {
        return parseSimpleStmt(lexer);
    }
}

Node *parseImportStmt(PyLexer &lexer) {
    if (lexer.curr->getType() == Python3Lexer::IMPORT) {
        return parseImportName(lexer);
    } else if (lexer.curr->getType() == Python3Lexer::FROM) {
        return parseImportFrom(lexer);
    } else {
        ERR_MSG_EXIT("expected IMPORT or FROM\n");
    }
}

Import *parseImportName(PyLexer &lexer) {
    lexer.consume(Python3Lexer::IMPORT);
    const auto aliases = parseDottedAsNames(lexer);
    auto import = new Import(aliases);

    return import;
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

Node *parseTestNoCond(PyLexer &lexer) {
    switch (lexer.curr->getType()) {
        case Python3Parser::STRING:
        case Python3Parser::NUMBER:
        case Python3Parser::NOT:
        case Python3Parser::NONE:
        case Python3Parser::TRUE:
        case Python3Parser::FALSE:
        case Python3Parser::AWAIT:
        case Python3Parser::NAME:
        case Python3Parser::ELLIPSIS:
        case Python3Parser::OPEN_PAREN:
        case Python3Parser::OPEN_BRACK:
        case Python3Parser::ADD:
        case Python3Parser::MINUS:
        case Python3Parser::NOT_OP:
        case Python3Parser::OPEN_BRACE: {
            return parseOrTest(lexer);
        }
        case Python3Lexer::LAMBDA:
            return parseLambDefNoCond(lexer);
        default:
        ERR_MSG_EXIT("Expected OR_TEST or LAMBDA");
    }
}

Node *parseLambdefNoCond(PyLexer &lexer) {
    return nullptr;
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

Argument *parseArgument(PyLexer &lexer) {
    switch (lexer.curr->getType()) {
        case Python3Parser::STRING:
        case Python3Parser::NUMBER:
        case Python3Parser::LAMBDA:
        case Python3Parser::NOT:
        case Python3Parser::NONE:
        case Python3Parser::TRUE:
        case Python3Parser::FALSE:
        case Python3Parser::AWAIT:
        case Python3Parser::NAME:
        case Python3Parser::ELLIPSIS:
        case Python3Parser::OPEN_PAREN:
        case Python3Parser::OPEN_BRACK:
        case Python3Parser::ADD:
        case Python3Parser::MINUS:
        case Python3Parser::NOT_OP:
        case Python3Parser::OPEN_BRACE: {
            const auto test = parseTest(lexer);
            if (lexer.curr->getType() == Python3Lexer::ASYNC ||
                lexer.curr->getType() == Python3Lexer::FOR) {

                const auto comprehension = parseCompFor(lexer);

            }
        }
    }

    const auto current_token = lexer.curr;
    lexer.consume(Python3Lexer::Python3Lexer::NAME);

    auto node = new Argument(new Name(current_token->getText()), nullptr, nullptr);
    if (lexer.curr->getType() == Python3Lexer::COLON) {
        lexer.consume(Python3Lexer::COLON);
        node->type = parseTest(lexer);
    }

    return node;
}

TryStmt *parseTryStmt(PyLexer &lexer) {
    return nullptr;
}

Node *parseDecorated(PyLexer &lexer) {
    return nullptr;
}

Node *parseAsyncStmt(PyLexer &lexer) {
    return nullptr;
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

ForStmt *parseForStmt(PyLexer &lexer) {
    return nullptr;
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

// testlist_star_expr: (test|star_expr) (',' (test|star_expr))* (',')?;
TestList *parseTestlistStarExpr(PyLexer &lexer) {
    auto test_list = new TestList({});
    Node *expr;
    if (lexer.curr->getType() == Python3Lexer::STAR) {
        expr = parseStarExpr(lexer);
    } else {
        expr = parseTest(lexer);
    }

    test_list->nodes.push_back(expr);

    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        Node *expr;
        if (lexer.curr->getType() == Python3Lexer::STAR) {
            expr = parseStarExpr(lexer);
        } else {
            expr = parseTest(lexer);
        }
        test_list->nodes.push_back(expr);
    }

    return test_list;
}

Global *parseGlobalStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::GLOBAL);

    auto global = new Global({});

    const auto current_token = lexer.curr;
    lexer.consume(Python3Lexer::NAME);
    const auto name = new Name(current_token->getText());

    global->names.push_back(name);
    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        lexer.consume(Python3Lexer::COMMA);
        const auto name = new Name(lexer.curr->getText());
        global->names.push_back(name);
    }

    return global;
}

Nonlocal *parseNonlocalStmt(PyLexer &lexer) {
    lexer.consume(Python3Lexer::NONLOCAL);

    auto nonlocal = new Nonlocal({});

    const auto current_token = lexer.curr;
    lexer.consume(Python3Lexer::NAME);
    const auto name = new Name(current_token->getText());

    nonlocal->names.push_back(name);
    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        lexer.consume(Python3Lexer::COMMA);
        const auto name = new Name(lexer.curr->getText());
        nonlocal->names.push_back(name);
    }

    return nonlocal;
}

// expr_stmt: testlist_star_expr (annassign | augassign (yield_expr|testlist) |
//                     ('=' (yield_expr|testlist_star_expr))*);
Node *parseExprStmt(PyLexer &lexer) {
    const auto test_list_star_expr = parseTestlistStarExpr(lexer);

    switch (lexer.curr->getType()) {
        case Python3Lexer::COLON: {
            const auto ann_assign = parseAnnAssign(lexer);
            ann_assign->target = test_list_star_expr;
            return ann_assign;
        }
        case Python3Lexer::ASSIGN: {
            auto assign = new Assign(nullptr, nullptr);
            assign->targets = test_list_star_expr;
            while (lexer.curr->getType() == Python3Lexer::ASSIGN) {
                lexer.consume(Python3Lexer::ASSIGN);
                if (lexer.curr->getType() == Python3Lexer::YIELD) {
                    assign->value = parseYieldExpr(lexer);
                } else {
                    assign->value = parseTestlistStarExpr(lexer);
                }
            }
            return assign;
        }
        default: {
            if (isAugAssign(lexer.curr)) {
                auto aug_assign = new AugAssign(nullptr, lexer.curr->getType(), nullptr);
                aug_assign->target = test_list_star_expr;
                lexer.consume(lexer.curr->getType());
                Node *value;
                if (lexer.curr->getType() == Python3Lexer::YIELD) {
                    value = parseYieldExpr(lexer);
                } else {
                    value = parseTestList(lexer);
                }
                aug_assign->value = value;
                return aug_assign;
            }
        }
    }

    // maybe, trigger crash
    ERR_MSG_EXIT("expected COLON, ASSIGN or AUG_ASSIGN");
}

AnnAssign *parseAnnAssign(PyLexer &lexer) {
    lexer.consume(Python3Lexer::COLON);

    auto ann_assign = new AnnAssign(nullptr, nullptr, nullptr);

    const auto annotation = parseTest(lexer);

    ann_assign->annotation = annotation;
    if (lexer.curr->getType() == Python3Lexer::ASSIGN) {
        lexer.consume(Python3Lexer::ASSIGN);

        const auto value = parseTest(lexer);
        ann_assign->value = value;
    }

    return ann_assign;
}

TestList *parseTestListStarExpr(PyLexer &lexer) {
    TestList *test_list = new TestList({});

    Node *node;
    if (lexer.curr->getType() == Python3Lexer::STAR) {
        node = parseStarExpr(lexer);
    } else {
        node = parseTest(lexer);
    }

    test_list->nodes.push_back(node);
    while (lexer.curr->getType() == Python3Lexer::COMMA) {
        lexer.consume(Python3Lexer::COMMA);

        Node *n;
        if (lexer.curr->getType() == Python3Lexer::STAR) {
            n = parseStarExpr(lexer);
        } else {
            n = parseTest(lexer);
        }
        test_list->nodes.push_back(n);
    }

    return test_list;
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
    if (lexer.curr->getType() == Python3Lexer::AWAIT) {
        lexer.consume(Python3Lexer::AWAIT);
    }

}

Node *parseTestlistComp(PyLexer &lexer) {
    return nullptr;
}

Node *parseAtom(PyLexer &lexer) {
    Node *node;
    const auto current_token = lexer.curr;
    switch (current_token->getType()) {
        case Python3Lexer::OPEN_PAREN: {
            lexer.consume(Python3Lexer::OPEN_PAREN);

            switch (lexer.curr->getType()) {
                case Python3Lexer::YIELD:
                    node = parseYieldExpr(lexer);
                    break;
                default:
                    if (isTestlistComp(lexer.curr))
                        node = parseTestlistComp(lexer);
                    break;
            }

            lexer.consume(Python3Lexer::CLOSE_PAREN);
            break;
        }
        case Python3Lexer::OPEN_BRACK: {
            lexer.consume(Python3Lexer::OPEN_BRACK);

            if (isTestlistComp(lexer.curr))
                node = parseTestlistComp(lexer);

            lexer.consume(Python3Lexer::CLOSE_BRACK);
            break;
        }
        case Python3Lexer::OPEN_BRACE: {
            lexer.consume(Python3Lexer::OPEN_BRACE);

            switch (lexer.curr->getType()) {
                case Python3Parser::STRING:
                case Python3Parser::NUMBER:
                case Python3Parser::LAMBDA:
                case Python3Parser::NOT:
                case Python3Parser::NONE:
                case Python3Parser::TRUE:
                case Python3Parser::FALSE:
                case Python3Parser::AWAIT:
                case Python3Parser::NAME:
                case Python3Parser::ELLIPSIS:
                case Python3Parser::OPEN_PAREN:
                case Python3Parser::OPEN_BRACK:
                case Python3Parser::ADD:
                case Python3Parser::MINUS:
                case Python3Parser::NOT_OP:
                case Python3Parser::OPEN_BRACE:
                case Python3Parser::POWER:
                    node = parseDictorsetmaker(lexer);
                    break;
            }

            lexer.consume(Python3Lexer::CLOSE_BRACE);
        }

        case Python3Lexer::NUMBER: {
            lexer.consume(Python3Lexer::NUMBER);
            node = new Const(current_token->getText(), Python3Lexer::NUMBER);
            break;
        }

        case Python3Lexer::STRING: {
            lexer.consume(Python3Lexer::STRING);
            node = new Const(current_token->getText(), Python3Lexer::STRING);
            break;
        }

        case Python3Lexer::NAME: {
            lexer.consume(Python3Lexer::NAME);
            node = new Name(current_token->getText());
            break;
        }

        case Python3Lexer::ELLIPSIS: {
            lexer.consume(Python3Lexer::ELLIPSIS);
            break;
        }

        case Python3Lexer::NONE: {
            lexer.consume(Python3Lexer::NONE);
            break;
        }
        default:
        ERR_MSG_EXIT("Encountered an unknown node");
    }

    return node;
}


Node *parseCompFor(PyLexer &lexer) {
    auto comp_for = new Comprehension(nullptr, nullptr, {}, false);
    if (lexer.curr->getType() == Python3Lexer::ASYNC) {
        lexer.consume(Python3Lexer::ASYNC);
        // TODO(threadedstream): please, handle all async cases
        comp_for->is_async = true;
    }

    lexer.consume(Python3Lexer::FOR);
    comp_for->target = parseExprList(lexer);

    lexer.consume(Python3Lexer::IN);
    comp_for->iter = parseOrTest(lexer);

    Node *temp;
    switch (lexer.curr->getType()) {
        case Python3Lexer::IF:
        case Python3Lexer::FOR:
            temp = parseCompIter(lexer);
            break;
        default: {
            if (lexer.curr->getType() == Python3Lexer::ASYNC &&
                lexer.next && lexer.next->getType() == Python3Lexer::FOR) {

                temp = parseCompIter(lexer);
            }
        }
            break;
    }

    // TODO(threadedstream): return anything else but nullptr
    return nullptr;
}

Node *parseDict(PyLexer &lexer) {

    const auto key = parseTest(lexer);
    lexer.consume(Python3Lexer::COLON);
    const auto value = parseTest(lexer);

    switch (lexer.curr->getType()) {
        case Python3Lexer::ASYNC:
        case Python3Lexer::FOR:
            auto dict = new DictComp(key, value, {});
            const auto comprehension = parseCompFor(lexer);
            dict->generators.push_back(comprehension);
    }

}

//(
//  (
//      (test ':' test | '**' expr)
//      (comp_for | (',' (test ':' test | '**' expr))* (',')?)
//  ) |
//  ((test | star_expr) (comp_for | (',' (test | star_expr))* (',')?))
// );
Node *parseDictorsetmaker(PyLexer &lexer) {
    Node *node;

    switch (lexer.curr->getType()) {
        case Python3Parser::STRING:
        case Python3Parser::NUMBER:
        case Python3Parser::LAMBDA:
        case Python3Parser::NOT:
        case Python3Parser::NONE:
        case Python3Parser::TRUE:
        case Python3Parser::FALSE:
        case Python3Parser::AWAIT:
        case Python3Parser::NAME:
        case Python3Parser::ELLIPSIS:
        case Python3Parser::OPEN_PAREN:
        case Python3Parser::OPEN_BRACK:
        case Python3Parser::ADD:
        case Python3Parser::MINUS:
        case Python3Parser::NOT_OP:
        case Python3Parser::OPEN_BRACE: {
            const auto key = parseTest(lexer);
            lexer.consume(Python3Lexer::COLON);
            const auto value = parseTest(lexer);

            switch (lexer.curr->getType()) {
                case Python3Lexer::ASYNC:
                case Python3Lexer::FOR: {
                    auto dict_comp = new DictComp(key, value, {});
                    while (lexer.curr->getType() == Python3Lexer::ASYNC ||
                           lexer.curr->getType() == Python3Lexer::FOR) {

                        const auto comprehension = parseCompFor(lexer);
                        dict_comp->generators.push_back(comprehension);
                    }
                }
            }
            break;
        }
        case Python3Lexer::POWER: {
            lexer.consume(Python3Lexer::POWER);
            const auto expr = parseExpr(lexer);
            return parseDict(lexer);
        }
        default: {
            ERR_MSG_EXIT("Expected TEST or POWER");
            break;
        }
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
    return parseDictorsetmaker(lexer);
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



