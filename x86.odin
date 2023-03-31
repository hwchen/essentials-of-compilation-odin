package ess

X86 :: struct {
    instructions: [dynamic]Instruction,
}

Instruction :: union {
    Addq,
    Subq,
    Movq,
    Negq,
    Pushq,
    Popq,
    Callq,
    Retq,
    Jump,
}

Addq :: distinct [2]Arg
Subq :: distinct [2]Arg
Movq :: distinct [2]Arg
Negq :: distinct Arg
Pushq :: distinct Arg
Popq :: distinct Arg
Callq :: struct {
    label: string,
    arity: int,
}
Retq :: struct {}
Jump :: distinct string

Arg :: union {
    Immediate,
    Reg,
    Deref,
}

Immediate :: distinct int
Deref :: struct {
    reg:    Reg,
    offset: int,
}
Reg :: enum {
    rsp,
    rbp,
    rax,
    rbx,
    rcs,
    rdx,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
}
