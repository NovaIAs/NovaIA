```
.segment .text
.global main
main:
    ; Load the value 10 into the EAX register
    mov eax, 10

    ; Print the string "Hello, world!" to the console
    mov edx, msg
    mov ecx, msg_len
    mov ebx, 1 ; Standard output
    mov eax, 4 ; System call number for write
    int 0x80

    ; Exit the program with a return code of 0
    mov eax, 0
    mov ebx, 0
    int 0x80

.data
msg: db "Hello, world!", 10
msg_len: equ $ - msg
```

Explanation:

The code starts by defining a segment called `.text` which will contain the machine instructions.

The `main` function is defined as the entry point of the program.

The `mov` instruction is used to load a value into a register. In this case, the value 10 is loaded into the EAX register.

The `mov` instruction is also used to load the address of the string "Hello, world!" into the EDX register.

The `mov` instruction is used to load the length of the string "Hello, world!" into the ECX register.

The `mov` instruction is used to load the value 1 (standard output) into the EBX register.

The `mov` instruction is used to load the system call number for `write` (4) into the EAX register.

The `int` instruction is used to make a system call. In this case, the system call `write` is used to print the string "Hello, world!" to the console.

The `mov` instruction is used to load the value 0 into the EAX register.

The `mov` instruction is used to load the value 0 into the EBX register.

The `int` instruction is used to make a system call. In this case, the system call `exit` is used to exit the program with a return code of 0.

The data segment contains the string "Hello, world!".

The `equ` directive is used to define a symbolic constant called `msg_len` which is equal to the length of the string "Hello, world!".