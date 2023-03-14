package ess

import "core:fmt"
import "core:strconv"
import "core:strings"
import "core:testing"

// Lexer ===================

Token :: struct {
    kind:  TokenKind,
    start: int,
    end:   int, //exclusive, used to slice [)
}

TokenKind :: enum {
    Print,
    InputInt,
    Ident,
    Number,
    Plus,
    Minus,
    Eq,
    LParen,
    RParen,
    Semicolon,
}

lex :: proc(src: string) -> [dynamic]Token {
    toks := make([dynamic]Token, 0)

    for idx := 0; idx < len(src); idx += 1 {
        switch src[idx] {
        case ' ', '\t', '\n':
            continue
        case '+':
            append(&toks, Token{kind = .Plus, start = idx, end = idx + 1})
        case '-':
            append(&toks, Token{kind = .Minus, start = idx, end = idx + 1})
        case '=':
            append(&toks, Token{kind = .Eq, start = idx, end = idx + 1})
        case '(':
            append(&toks, Token{kind = .LParen, start = idx, end = idx + 1})
        case ')':
            append(&toks, Token{kind = .RParen, start = idx, end = idx + 1})
        case ';':
            append(&toks, Token{kind = .Semicolon, start = idx, end = idx + 1})
        case '0' ..= '9':
            start := idx
            num_loop: for idx < len(src) {
                switch src[idx] {
                case '0' ..= '9':
                    idx += 1
                    continue
                case:
                    break num_loop
                }
            }
            idx -= 1
            append(&toks, Token{kind = .Number, start = start, end = idx + 1})
        case 'a' ..= 'z', 'A' ..= 'Z', '_':
            start := idx
            word_loop: for idx < len(src) {
                switch src[idx] {
                case 'a' ..= 'z', 'A' ..= 'Z', '_':
                    idx += 1
                    continue
                case:
                    break word_loop
                }
            }
            idx -= 1
            end := idx + 1
            switch src[start:end] {
            case "print":
                append(&toks, Token{kind = .Print, start = start, end = end})
            case "input_int":
                append(&toks, Token{kind = .InputInt, start = start, end = end})
            case:
                append(&toks, Token{kind = .Ident, start = start, end = end})
            }
        // TODO
        case:
        }
    }
    return toks
}

@(test)
smoke_test_lexer_1 :: proc(t: ^testing.T) {
    src := "a = 1 + (input_int() - 3);"
    toks := lex(src)

    testing.expect_value(t, toks[0].kind, TokenKind.Ident)
    testing.expect_value(t, toks[1].kind, TokenKind.Eq)
    testing.expect_value(t, toks[2].kind, TokenKind.Number)
    testing.expect_value(t, toks[3].kind, TokenKind.Plus)
    testing.expect_value(t, toks[4].kind, TokenKind.LParen)
    testing.expect_value(t, toks[5].kind, TokenKind.InputInt)
    testing.expect_value(t, toks[6].kind, TokenKind.LParen)
    testing.expect_value(t, toks[7].kind, TokenKind.RParen)
    testing.expect_value(t, toks[8].kind, TokenKind.Minus)
    testing.expect_value(t, toks[9].kind, TokenKind.Number)
    testing.expect_value(t, toks[10].kind, TokenKind.RParen)
    testing.expect_value(t, toks[11].kind, TokenKind.Semicolon)
}

@(test)
smoke_test_lexer_2 :: proc(t: ^testing.T) {
    src := "a + b"
    toks := lex(src)
    testing.expect_value(t, src[toks[0].start:toks[0].end], "a")
    testing.expect_value(t, src[toks[2].start:toks[2].end], "b")
}

// Parser ===================

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
        fmt.println("curr at beginning of stmt parse:", state.curr)
        append(&stmts, parse_stmt(&state) or_return)
        fmt.println("stmt appended")
        consume_expect_tok(.Semicolon, &state) or_return
        fmt.println("semicolon consumed", state.curr)
    }

    return Ast{stmts = stmts}, nil
}

parse_stmt :: proc(state: ^ParseState) -> (stmt: Stmt, err: ParseError) {
    if match_tok(.Print, state) {
        consume_expect_tok(.LParen, state) or_return
        expr := parse_expr(state) or_return
        consume_expect_tok(.RParen, state) or_return
        return transmute(StmtPrint)expr, nil
    } else if peek_match_tok(.Eq, state) {
        // We only need to lookahead one tok, since we've lexed Eq and DoubleEq as separate toks
        variable := curr_tok_slice(state)
        state.curr += 2
        expr := parse_expr(state) or_return
        return StmtAssign{variable = variable, expr = expr}, nil
    } else {
        expr := parse_expr(state) or_return
        return transmute(StmtExpr)expr, nil
    }
}

// Pratt parser
parse_expr :: proc(state: ^ParseState) -> (expr: Expr, err: ParseError) {
    return expr_bp(state, 0)
}

expr_bp :: proc(state: ^ParseState, min_bp: int) -> (expr: Expr, err: ParseError) {
    fmt.println("ENTER expr_bp", state.curr)
    lhs: Expr
    #partial switch tok := consume_tok(state); tok.kind {
    case .Number:
        fmt.println("  SWITCH NUMBER expr_bp", state.curr)
        n, _ := strconv.parse_int(tok_slice(tok, state))
        lhs = transmute(Number)n
    case .Ident:
        fmt.println("  SWITCH IDENT expr_bp", state.curr)
        lhs = transmute(Variable)tok_slice(tok, state)
    case .Plus, .Minus:
        fmt.println("  SWITCH OP expr_bp", state.curr)
        rhs, _ := expr_bp(state, 5)
        rhs_ptr := new(Expr)
        rhs_ptr^ = rhs
        lhs = UnaryOp {
            op   = tok2op(tok.kind),
            expr = rhs_ptr,
        }
    case .LParen:
        lhs, _ = expr_bp(state, 0)
        consume_expect_tok(.RParen, state) or_return
    case:
        return nil, ParseError.UnexpectedToken
    }

    for {
        fmt.println("  LOOP expr_bp", state.curr, lhs)
        if is_eof(state) || match_tok(.Semicolon, state) || match_tok(.RParen, state) do break

        // hardcode here, since we only need binding power for +-
        l_bp := 1
        r_bp := 2

        if l_bp < min_bp do break

        op := consume_tok(state)
        fmt.println("  LOOP OP expr_bp", state.curr, op)

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
    fmt.println("  LOOP EXIT expr_bp", state.curr, lhs)

    return lhs, nil
}

tok2op :: proc(op: TokenKind) -> Op {
    #partial switch op {
    case .Plus:
        return .Add
    case .Minus:
        return .Sub
    case:
        fmt.println(op)
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
    testing.expect_value(t, ast_debug(ast), "(StmtExpr (+ 1 (- (+ 1 1) 1)))")
}
//{
//    ast, _ := parse("1+ -1);")
//    testing.expect_value(t, ast_debug(ast), "(StmtExpr (+ 1 (- 1))")
//}

// AST ===================

Ast :: struct {
    stmts: [dynamic]Stmt,
}

Stmt :: union {
    StmtPrint,
    StmtExpr,
    StmtAssign,
}
StmtPrint :: distinct Expr
StmtExpr :: distinct Expr
StmtAssign :: struct {
    variable: string,
    expr:     Expr,
}

Expr :: union {
    Number,
    Variable,
    BinaryOp,
    UnaryOp,
    Group,
    InputInt,
}
Number :: distinct int
Variable :: distinct string
BinaryOp :: struct {
    lhs: ^Expr,
    op:  Op,
    rhs: ^Expr,
}
UnaryOp :: struct {
    op:   Op,
    expr: ^Expr,
}
Group :: distinct ^Expr
InputInt :: distinct bool // void type, bool is not use

Op :: enum {
    Add,
    Sub,
}

ast_debug :: proc(ast: Ast) -> string {
    buf := strings.builder_make_none()
    for stmt in ast.stmts {
        stmt_debug(&buf, stmt)
    }
    return strings.to_string(buf)
}

stmt_debug :: proc(buf: ^strings.Builder, stmt: Stmt) {
    switch s in stmt {
    case StmtPrint:
        fmt.sbprintf(buf, "(StmtPrint ")
        expr_debug(buf, transmute(Expr)s)
        fmt.sbprintf(buf, ")")
    case StmtExpr:
        fmt.sbprintf(buf, "(StmtExpr ")
        expr_debug(buf, transmute(Expr)s)
        fmt.sbprintf(buf, ")")
    case StmtAssign:
        fmt.sbprintf(buf, "(StmtAssign %s ", s.variable)
        expr_debug(buf, s.expr)
        fmt.sbprintf(buf, ")", s.variable)
    }
}

expr_debug :: proc(buf: ^strings.Builder, expr: Expr) {
    switch e in expr {
    case Number:
        fmt.sbprintf(buf, "%d", e)
    case Variable:
        fmt.sbprintf(buf, "%s", e)
    case BinaryOp:
        fmt.sbprintf(buf, "(")
        op_debug(buf, e.op)
        fmt.sbprintf(buf, " ")
        expr_debug(buf, e.lhs^)
        fmt.sbprintf(buf, " ")
        expr_debug(buf, e.rhs^)
        fmt.sbprintf(buf, ")")
    case UnaryOp:
        fmt.sbprintf(buf, "(")
        op_debug(buf, e.op)
        fmt.sbprintf(buf, " ")
        expr_debug(buf, e.expr^)
        fmt.sbprintf(buf, ")")
    case Group:
        fmt.sbprintf(buf, "(Group ")
        expr_debug(buf, e^)
        fmt.sbprintf(buf, " )")
    case InputInt:
        fmt.sbprintf(buf, "(input_int)")
    }
}

op_debug :: proc(buf: ^strings.Builder, op: Op) {
    switch op {
    case .Add:
        fmt.sbprintf(buf, "+")
    case .Sub:
        fmt.sbprintf(buf, "-")
    }
}
