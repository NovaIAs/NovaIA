```assembly
.global _start

_start:
    mov eax, 4            ; System call number for "write"
    mov ebx, 1            ; File descriptor for stdout
    mov ecx, message      ; Address of the message to write
    mov edx, message_len  ; Length of the message
    int 0x80              ; Make the system call

    mov eax, 1            ; System call number for "exit"
    mov ebx, 0            ; Exit code
    int 0x80              ; Make the system call

message:
    .asciz "Hello, world!\n"
message_len:
    .int 13
```

This program prints the message "Hello, world!" to the console. It uses the `write` system call to write the message to the standard output file descriptor, and then the `exit` system call to exit the program with a status code of 0.

The program is written in assembly language, which is a low-level programming language that is directly translated into machine code. This makes assembly language very efficient, but also very difficult to read and write.

Here is a brief explanation of the code:

* The `.global _start` directive tells the assembler that the `_start` label is a global symbol, which means that it can be referenced from other parts of the program.
* The `_start:` label marks the beginning of the program.
* The `mov` instruction is used to move data from one register to another. In this program, the `mov` instruction is used to move the system call number, file descriptor, address of the message, and length of the message into the appropriate registers.
* The `int` instruction is used to make a system call. In this program, the `int` instruction is used to make the `write` and `exit` system calls.
* The `message` and `message_len` labels are used to define the message to be printed and the length of the message, respectively.

This program is a very simple example of an assembly language program. However, it demonstrates some of the basic concepts of assembly language programming, such as using labels, registers, and system calls.