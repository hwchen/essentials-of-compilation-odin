test:
    watchexec --exts odin -- odin test .

run:
    odin build . -o:speed

check-leak testfile:
    odin build . -o:speed && valgrind --leak-check=full ./essentials-of-compilation-odin.bin -- {{testfile}}
