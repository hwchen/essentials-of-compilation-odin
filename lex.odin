package ess

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
