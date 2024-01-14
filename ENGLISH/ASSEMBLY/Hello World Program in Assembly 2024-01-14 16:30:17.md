```assembly
.model small
.stack 100h

.data
message db 'Hello, world!', 13, 10, '$'
message_len equ $-message

.code
main proc
    mov ax, @data
    mov ds, ax

    mov ah, 9
    mov dx, offset message
    mov cx, message_len
    int 21h

    mov ah, 4ch
    int 21h

main endp
end main
```

This code is a simple program that prints "Hello, world!" to the console. It uses the DOS interrupt 21h to perform input and output operations.

The program starts by setting up the data segment register (DS) to point to the data segment, which contains the message to be printed. It then uses the interrupt 21h with function 9 to print the message to the console. Finally, it uses interrupt 21h with function 4ch to exit the program.

Here is a breakdown of the code:

* `.model small`: This directive tells the assembler that we are using the small memory model, which means that the program will be loaded into memory at a low address.
* `.stack 100h`: This directive reserves 100h (256 bytes) of memory for the stack.
* `.data`: This directive starts the data segment.
* `message db 'Hello, world!', 13, 10, '$'`: This defines a data byte array called `message` that contains the message "Hello, world!" followed by a carriage return (13), a line feed (10), and a dollar sign ($). The dollar sign is used to mark the end of the string.
* `message_len equ $-message`: This defines a constant called `message_len` that is equal to the length of the `message` array.
* `.code`: This directive starts the code segment.
* `main proc`: This defines the `main` procedure, which is the entry point of the program.
* `mov ax, @data`: This instruction moves the address of the data segment into the AX register.
* `mov ds, ax`: This instruction moves the contents of the AX register into the DS register, which sets the data segment register to point to the data segment.
* `mov ah, 9`: This instruction moves the value 9 into the AH register. This value tells interrupt 21h that we want to print a string to the console.
* `mov dx, offset message`: This instruction moves the offset of the `message` array into the DX register. This tells interrupt 21h where to find the string to print.
* `mov cx, message_len`: This instruction moves the length of the `message` array into the CX register. This tells interrupt 21h how long the string is.
* `int 21h`: This instruction calls interrupt 21h with the function code 9. This causes the message to be printed to the console.
* `mov ah, 4ch`: This instruction moves the value 4ch into the AH register. This value tells interrupt 21h that we want to exit the program.
* `int 21h`: This instruction calls interrupt 21h with the function code 4ch. This causes the program to exit.
* `main endp`: This directive marks the end of the `main` procedure.
* `end main`: This directive marks the end of the program.