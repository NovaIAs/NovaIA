```assembly
    section .text
    global _start

_start:
    mov eax, 4       ; system call for write
    mov ebx, 1       ; file descriptor for stdout
    mov ecx, message ; address of the message to write
    mov edx, len      ; length of the message
    int 0x80         ; invoke the system call

    mov eax, 1       ; system call for exit
    mov ebx, 0       ; exit code
    int 0x80         ; invoke the system call

section .data
message: db "Hello, world!", 10
len: equ $ - message
```

This program prints the message "Hello, world!" to the standard output.

The program starts by defining a section called `.text`, which contains the executable code of the program. The global keyword is used to specify that the _start symbol, which is the entry point of the program, can be accessed from other parts of the program.

The _start label marks the beginning of the code. The mov instruction is used to move values into registers. The eax register is used to specify the system call number, the ebx register is used to specify the file descriptor, the ecx register is used to specify the address of the data to write, and the edx register is used to specify the length of the data to write.

The int instruction is used to invoke a system call. The argument to the int instruction is the system call number. In this case, the system call number is 4, which corresponds to the write system call.

After the write system call has been invoked, the program calls the exit system call to terminate the program. The argument to the exit system call is the exit code. In this case, the exit code is 0, which indicates that the program terminated successfully.

The program also defines a section called `.data`, which contains the data that is used by the program. In this case, the data section contains a message that is printed to the standard output.

The message label marks the beginning of the message. The db directive is used to define a byte array. The 10 at the end of the message is a newline character.

The len label is used to specify the length of the message. The equ directive is used to define a symbol that is equal to the difference between the current address and the address of the message label.

This program is a simple example of an assembly language program that prints a message to the standard output.