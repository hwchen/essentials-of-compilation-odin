package ess

import "core:fmt"
import "core:testing"

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
Number :: distinct i64
Variable :: distinct string
BinaryOp :: struct {
    lhs: ^Expr,
    op:  OpCode,
    rhs: ^Expr,
}
UnaryOp :: struct {
    op:   OpCode,
    expr: ^Expr,
}
Group :: distinct ^Expr
InputInt :: distinct bool // void type, bool is not use

OpCode :: enum {
    Add,
    Sub,
}

ast_print :: proc(ast: Ast) {
    for stmt in ast.stmts {
        stmt_print(stmt)
        fmt.printf(" ;\n")
    }
}

stmt_print :: proc(stmt: Stmt) {
    switch s in stmt {
    case StmtPrint:
        fmt.printf("Stmt Print: ")
        expr_print(transmute(Expr)s)
    case StmtExpr:
        fmt.printf("Stmt Expr: ")
        expr_print(transmute(Expr)s)
    case StmtAssign:
        fmt.printf("Stmt Assign: %s", s.variable)
        expr_print(s.expr)
    }
}

expr_print :: proc(expr: Expr) {
    switch e in expr {
    case Number:
        fmt.printf("%d", e)
    case Variable:
        fmt.printf("%s", e)
    case BinaryOp:
        expr_print(e.lhs^)
        fmt.printf(" ")
        opcode_print(e.op)
        fmt.printf(" ")
        expr_print(e.rhs^)
    case UnaryOp:
        opcode_print(e.op)
        expr_print(e.expr^)
    case Group:
        fmt.printf("( ")
        expr_print(e^)
        fmt.printf(" )")
    case InputInt:
        fmt.printf("input_int")
    }
}

opcode_print :: proc(op: OpCode) {
    switch op {
    case .Add:
        fmt.printf("+")
    case .Sub:
        fmt.printf("-")
    }
}
