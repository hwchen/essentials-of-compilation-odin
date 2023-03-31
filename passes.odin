package ess

passes :: proc(ast: Ast) -> X86 {
    ast := ast
    remove_complex_operands(&ast)

    x86 := select_instructions(ast)
    assign_homes(&x86)
    patch_instructions(&x86)
    prelude_and_conclusion(&x86)

    return x86
}

remove_complex_operands :: proc(ast: ^Ast) {}
select_instructions :: proc(ast_mon: Ast) -> X86 {
    return X86{}
}
assign_homes :: proc(x86: ^X86) {}
patch_instructions :: proc(x86: ^X86) {}
prelude_and_conclusion :: proc(x86: ^X86) {}
