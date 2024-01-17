```assembly
; This program demonstrates the use of macros and procedures in assembly language.

; Define a macro to print a string.
.macro print_str string
    mov dx, offset string
    mov ah, 9
    int 21h
.endm

; Define a procedure to calculate the factorial of a number.
.proc factorial number
    cmp number, 0
    je @done
    mov ax, number
    dec ax
    call factorial
    mul ax, number
@done:
    ret
.endp

; Define the data segment.
.data
    msg1:  .asciz "Enter a number: "
    msg2:  .asciz "The factorial of the number is: "

; Define the code segment.
.code
    ; Get the input from the user.
    mov ax, @data
    mov ds, ax
    mov ah, 9
    lea dx, msg1
    int 21h
    mov ah, 1
    int 21h
    sub al, 30h
    mov number, al

    ; Calculate the factorial of the number.
    mov ax, number
    call factorial

    ; Display the result.
    mov ax, @data
    mov ds, ax
    mov ah, 9
    lea dx, msg2
    int 21h
    mov ah, 2
    mov dl, al
    add dl, 30h
    int 21h

    ; Exit the program.
    mov ah, 4ch
    int 21h
```

This program first defines a macro called `print_str` which is used to print a string to the console. It then defines a procedure called `factorial` which calculates the factorial of a number. The program then defines the data segment, which contains the strings that will be printed to the console, and the code segment, which contains the instructions that will be executed.

The program first gets the input from the user using the `int 21h` interrupt with the `ah` register set to 1. The program then subtracts 30h from the input value to convert it from an ASCII character to a numeric value. The program then calls the `factorial` procedure to calculate the factorial of the number.

The program then prints the result to the console using the `print_str` macro and the `int 21h` interrupt with the `ah` register set to 9. The program then exits using the `int 21h` interrupt with the `ah` register set to 4ch.