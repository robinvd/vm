.code
    start: push 1
    jmpz true
    jmp false

    true: push 1
    halt
    false: push 2
    halt