```assembly
.code32

.global main

main:
    mov eax, 4
    mov ebx, 1
    mov ecx, message
    mov edx, lengthofmessage
    int 0x80

    mov eax, 1
    mov ebx, 0
    int 0x80

section .data
message: db "Hello, world!", 10
lengthofmessage: equ $ - message
```

Explanation:

* The first line, `.code32`, specifies that the code is for a 32-bit processor.
* The second line, `.global main`, makes the `main` function visible to other parts of the program.
* The `main` function starts at the third line.
* The fourth line, `mov eax, 4`, sets the `eax` register to 4. This is the system call number for `write`.
* The fifth line, `mov ebx, 1`, sets the `ebx` register to 1. This is the file descriptor for standard output.
* The sixth line, `mov ecx, message`, sets the `ecx` register to the address of the `message` variable.
* The seventh line, `mov edx, lengthofmessage`, sets the `edx` register to the length of the `message` variable.
* The eighth line, `int 0x80`, makes a system call. The system call number is in the `eax` register, the file descriptor is in the `ebx` register, the address of the data to write is in the `ecx` register, and the length of the data to write is in the `edx` register.
* The ninth line, `mov eax, 1`, sets the `eax` register to 1. This is the system call number for `exit`.
* The tenth line, `mov ebx, 0`, sets the `ebx` register to 0. This is the exit status.
* The eleventh line, `int 0x80`, makes a system call. The system call number is in the `eax` register, and the exit status is in the `ebx` register.
* The section directive `.data` starts the data section of the program.
* The `message` variable is a string literal. The `10` at the end of the string is a newline character.
* The `lengthofmessage` variable is an equ directive. It sets the value of the variable to the size of the `message` variable.