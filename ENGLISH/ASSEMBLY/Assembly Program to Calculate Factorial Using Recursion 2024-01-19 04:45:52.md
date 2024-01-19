```assembly
; This program calculates the factorial of a number using recursion.
; The input is a number in register AL, and the output is the factorial in register AX.

.model small
.stack 1024

.data

; Define a table of factorials.
factorial_table:
    dw 1
    dw 1
    dw 2
    dw 6
    dw 24
    dw 120
    dw 720
    dw 5040
    dw 40320
    dw 362880

; Define a message to print.
message:
    db "The factorial of ", 10, '$', 10, 0

.code

; Main procedure.
main proc

    ; Get the input number.
    mov al, [si]

    ; Check if the number is negative.
    cmp al, 0
    jl error

    ; Check if the number is greater than 9.
    cmp al, 9
    jg error

    ; Calculate the factorial of the number.
    call calculate_factorial

    ; Print the factorial.
    mov ah, 9
    mov dx, offset message
    int 21h

    ; Exit the program.
    mov ax, 4C00h
    int 21h

; Error procedure.
error proc

    ; Print an error message.
    mov ah, 9
    mov dx, offset error_message
    int 21h

    ; Exit the program.
    mov ax, 4C00h
    int 21h

; Calculate factorial procedure.
calculate_factorial proc

    ; Check if the number is 0 or 1.
    cmp al, 1
    jle return_one

    ; Get the factorial of the number from the table.
    mov bx, offset factorial_table
    mov bl, al
    shl bl, 1
    add bx, bl
    mov ax, [bx]

    ; Multiply the factorial by the number.
    mul al

    ; Return the factorial.
    ret

; Return one procedure.
return_one proc

    ; Set the factorial to 1.
    mov ax, 1

    ; Return the factorial.
    ret

endp main
```

This program uses a table of factorials to calculate the factorial of a number. The table is indexed by the number, and the factorial is stored in the corresponding word in the table. The program first checks if the number is negative or greater than 9. If it is, the program prints an error message and exits. Otherwise, the program gets the factorial of the number from the table and multiplies it by the number. The result is the factorial of the number, which is then printed.