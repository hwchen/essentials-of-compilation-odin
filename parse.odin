package ess

import "core:log"
import "core:strconv"
import "core:testing"

ParseError :: enum {
    UnexpectedToken,
}

ParseState :: struct {
    curr: int,
    toks: []Token,
    src:  string,
}

parse :: proc(src: string) -> (ast: Ast, err: ParseError) {
    toks := lex(src)

    state := ParseState {
        curr = 0,
        toks = toks[:],
        src  = src,
    }

    stmts := make([dynamic]Stmt, 0)
    for !is_eof(&state) {
        log.debug("curr at beginning of stmt parse:", state.curr)
        append(&stmts, parse_stmt(&state) or_return)
        log.debug("stmt appended")
        consume_expect_tok(.Semicolon, &state) or_return
        log.debug("semicolon consumed", state.curr)
    }

    return Ast{stmts = stmts}, nil
}

parse_stmt :: proc(state: ^ParseState) -> (stmt: Stmt, err: ParseError) {
    if match_tok(.Print, state) {
        consume_expect_tok(.Print, state) or_return
        consume_expect_tok(.LParen, state) or_return
        expr := parse_expr(state) or_return
        consume_expect_tok(.RParen, state) or_return
        return StmtPrint(expr), nil
    } else if peek_match_tok(.Eq, state) {
        // We only need to lookahead one tok, since we've lexed Eq and DoubleEq as separate toks
        variable := curr_tok_slice(state)
        state.curr += 2
        expr := parse_expr(state) or_return
        return StmtAssign{variable = variable, expr = expr}, nil
    } else {
        expr := parse_expr(state) or_return
        return StmtExpr(expr), nil
    }
}

// Pratt parser
parse_expr :: proc(state: ^ParseState) -> (expr: Expr, err: ParseError) {
    return expr_bp(state, 0)
}

expr_bp :: proc(state: ^ParseState, min_bp: int) -> (expr: Expr, err: ParseError) {
    log.debug("ENTER expr_bp", state.curr)
    lhs: Expr
    #partial switch tok := consume_tok(state); tok.kind {
    case .Number:
        log.debug("  SWITCH NUMBER expr_bp", state.curr)
        n, _ := strconv.parse_int(tok_slice(tok, state))
        lhs = Number(n)
    case .Ident:
        log.debug("  SWITCH IDENT expr_bp", state.curr)
        lhs = Variable(tok_slice(tok, state))
    case .Plus, .Minus:
        log.debug("  SWITCH OP expr_bp", state.curr)
        expr, _ := expr_bp(state, 5)
        expr_ptr := new(Expr)
        expr_ptr^ = expr
        lhs = UnaryOp {
            op   = tok2op(tok.kind),
            expr = expr_ptr,
        }
    case .LParen:
        log.debug("  SWITCH OP expr_bp", state.curr)
        expr, _ := expr_bp(state, 0)
        expr_ptr := new(Expr)
        expr_ptr^ = expr
        lhs = Group(expr_ptr)
        consume_expect_tok(.RParen, state) or_return
    case .InputInt:
        lhs = InputInt({})
        consume_expect_tok(.LParen, state) or_return
        consume_expect_tok(.RParen, state) or_return
    case:
        return nil, ParseError.UnexpectedToken
    }

    for {
        log.debug("  LOOP expr_bp", state.curr, lhs)
        if is_eof(state) || match_tok(.Semicolon, state) || match_tok(.RParen, state) do break

        // hardcode here, since we only need binding power for +-
        l_bp := 1
        r_bp := 2

        if l_bp < min_bp do break

        op := consume_tok(state)
        log.debug("  LOOP OP expr_bp", state.curr, op)

        rhs := expr_bp(state, r_bp) or_return
        rhs_ptr := new(Expr)
        rhs_ptr^ = rhs
        lhs_ptr := new(Expr)
        lhs_ptr^ = lhs
        lhs = BinaryOp {
            lhs = lhs_ptr,
            op  = tok2op(op.kind),
            rhs = rhs_ptr,
        }
    }
    log.debug("  LOOP EXIT expr_bp", state.curr, lhs)

    return lhs, nil
}

tok2op :: proc(op: TokenKind) -> Op {
    #partial switch op {
    case .Plus:
        return .Add
    case .Minus:
        return .Sub
    case:
        log.debug(op)
        panic("Token is not an op")
    }
}

consume_tok :: proc(state: ^ParseState) -> Token {
    tok := tok(state)
    state.curr += 1
    return tok
}

consume_expect_tok :: proc(tok_kind: TokenKind, state: ^ParseState) -> ParseError {
    if !match_tok(tok_kind, state) {
        return ParseError.UnexpectedToken
    } else {
        state.curr += 1
        return nil
    }
}

match_tok :: proc(tok_kind: TokenKind, state: ^ParseState) -> bool {
    return tok(state).kind == tok_kind
}

peek_match_tok :: proc(tok_kind: TokenKind, state: ^ParseState) -> bool {
    return state.toks[state.curr + 1].kind == tok_kind
}

tok :: proc(state: ^ParseState) -> Token {
    return state.toks[state.curr]
}

tok_slice :: proc(tok: Token, state: ^ParseState) -> string {
    return state.src[tok.start:tok.end]
}

curr_tok_slice :: proc(state: ^ParseState) -> string {
    return tok_slice(tok(state), state)
}

is_eof :: proc(state: ^ParseState) -> bool {
    return state.curr >= len(state.toks)
}

@(test)
test_parse_expr_basic :: proc(t: ^testing.T) {
    {
        ast, _ := parse("1+1;")
        testing.expect_value(t, ast_debug(ast), "(StmtExpr (+ 1 1))")
    }
    {
        ast, _ := parse("1+2-3;")
        testing.expect_value(t, ast_debug(ast), "(StmtExpr (- (+ 1 2) 3))")
    }
}

@(test)
test_parse_expr_group :: proc(t: ^testing.T) {
    ast, _ := parse("1+((1 + 1) - 1);")
    testing.expect_value(t, ast_debug(ast), "(StmtExpr (+ 1 (Group (- (Group (+ 1 1)) 1))))")
}

@(test)
test_parse_expr_unary :: proc(t: ^testing.T) {
    ast, _ := parse("1+ -1;")
    testing.expect_value(t, ast_debug(ast), "(StmtExpr (+ 1 (- 1)))")
}

@(test)
test_parse_stmt_assign :: proc(t: ^testing.T) {
    ast, _ := parse("a = 1;")
    testing.expect_value(t, ast_debug(ast), "(StmtAssign a 1)")
}

@(test)
test_parse_stmt_print :: proc(t: ^testing.T) {
    ast, _ := parse("print(1);")
    testing.expect_value(t, ast_debug(ast), "(StmtPrint 1)")
}

@(test)
test_parse_expr_input_int :: proc(t: ^testing.T) {
    //context.logger = log.create_console_logger(lowest = .Debug, ident = "test")
    ast, _ := parse("a = input_int();")
    log.debug(ast)
    testing.expect_value(t, ast_debug(ast), "(StmtAssign a input_int)")
}
