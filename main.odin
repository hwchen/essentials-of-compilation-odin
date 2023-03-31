package ess

import "core:bufio"
import "core:fmt"
import "core:mem"
import "core:os"

main :: proc() {
    context.allocator = context.temp_allocator
    defer free_all()

    // Note: os.args allocates, but is not cleaned up.
    // See https://github.com/odin-lang/Odin/issues/1633
    filepath: string
    if len(os.args) == 0 {
        panic("Must supply filepath arg")
    } else {
        filepath = os.args[1]
    }

    data, _ := os.read_entire_file(filepath)
    src := string(data)

    ast, _ := parse(src)
    fmt.println(ast_debug(ast))
}
