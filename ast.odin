package ess

import "core:fmt"
import "core:strings"

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
InputInt :: struct {}

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
        fmt.sbprintf(buf, ")")
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
        fmt.sbprintf(buf, ")")
    case InputInt:
        fmt.sbprintf(buf, "input_int")
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

