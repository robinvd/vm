.data
msg: "hello\n"
.code
push 1048576
copy 0
push 1
copy 0

loop: nop
    copy 0
    mul
    copy 2
    leq
    jmpz end
    const 1
    add
    copy 0
    jmp loop

end: nop
    halt

.comment
sqrt in code:

let x = 16
let i = 1
while i*i<16
    i += 1
return i