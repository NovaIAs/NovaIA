```assembly
.model small

.stack 100h

.data

msg: db 'Hello, world!', 13, 10, '$'

.code

main proc

    mov ax, @data
    mov ds, ax

    mov ah, 9
    mov dx, OFFSET msg
    int 21h

    mov ah, 4ch
    int 21h

main endp

end main
```

This code is a simple program that prints the message "Hello, world!" to the screen. It uses the DOS interrupt 21h to perform input and output operations.

The first few lines of the code define the program's data segment. The .data directive tells the assembler that the following lines contain data, rather than code. The msg label defines a string containing the message "Hello, world!", followed by a carriage return and a line feed character. The $ character at the end of the string is used to mark the end of the string.

The next few lines of the code define the program's code segment. The .code directive tells the assembler that the following lines contain code, rather than data. The main label defines the entry point of the program.

The first instruction in the main procedure is a mov instruction. This instruction moves the value of the @data directive into the ax register. The @data directive is a special symbol that represents the address of the start of the data segment.

The next instruction is a mov instruction. This instruction moves the value of the ax register into the ds register. The ds register is used to hold the address of the current data segment.

The next few instructions use the DOS interrupt 21h to perform input and output operations. The first instruction is a mov instruction. This instruction moves the value of 9 into the ah register. The ah register is used to specify the function number of the DOS interrupt. The value of 9 specifies the write string function.

The next instruction is a mov instruction. This instruction moves the address of the msg label into the dx register. The dx register is used to specify the address of the string to be written.

The next instruction is an int instruction. This instruction calls the DOS interrupt 21h. The DOS interrupt 21h will write the string specified by the dx register to the screen.

The next few instructions use the DOS interrupt 21h to terminate the program. The first instruction is a mov instruction. This instruction moves the value of 4ch into the ah register. The value of 4ch specifies the exit function.

The next instruction is an int instruction. This instruction calls the DOS interrupt 21h. The DOS interrupt 21h will terminate the program.