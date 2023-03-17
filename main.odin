package ess

import "core:bufio"
import "core:os"
import "core:fmt"

main :: proc() {
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
