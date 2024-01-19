```assembly
; Define the segment and the initial address
segment .data
address 0x1000

; Define the data in the segment
data db 'Hello, Assembly Programming!', 13, 10, '$'

; Define the code segment and the entry point
segment .code
entry main

; Define the main function
main:

    ; Load the address of the data into the DI register
    mov di, address

    ; Loop through the data and print it to the console
    mov cx, length_of_data
    loop:
        mov al, [di]
        call print_char
        inc di
        loop loop

    ; Exit the program
    mov ax, 4C00h
    int 21h

; Define the length of the data
length_of_data equ $ - data

; Define the function to print a character to the console
print_char:

    ; Save the registers that we will be using
    push ax
    push bx
    push cx
    push dx

    ; Move the character to the AL register
    mov al, [bp+4]

    ; Print the character to the console
    mov ah, 02h
    int 21h

    ; Restore the registers that we saved
    pop dx
    pop cx
    pop bx
    pop ax

    ; Return from the function
    ret
```

This code is a simple program that prints the message "Hello, Assembly Programming!" to the console. The code is written in assembly language and uses the Intel x86 instruction set.

The code starts by defining the segment and the initial address of the data. The data segment contains the message that will be printed to the console. The code then defines the code segment and the entry point of the program.

The main function of the program is responsible for printing the message to the console. The function starts by loading the address of the data into the DI register. The function then uses a loop to iterate through the data and print each character to the console.

The print_char function is responsible for printing a single character to the console. The function takes the character to be printed as an argument and uses the DOS interrupt 21h to print the character to the console.

The program ends by calling the DOS interrupt 4C00h to exit the program.