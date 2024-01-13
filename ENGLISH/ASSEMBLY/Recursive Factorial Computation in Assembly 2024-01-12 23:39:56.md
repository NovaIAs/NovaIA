```
Title: Computing Factorial with a Recursive Procedure in Assembly

.model small
.stack 1024

.data
prompt1 byte "Enter a non-negative integer: ", 0
prompt2 byte "Factorial of ", 0
result byte " is ", 0
newline byte 13, 10, '$'

.code
main proc
    mov ax, @data
    mov ds, ax

    ; Display the prompt
    mov ah, 9
    mov dx, offset prompt1
    int 21h

    ; Get the input from the user
    mov ah, 1
    int 21h
    mov bl, al
    sub bl, '0'

    ; Check if the input is valid
    cmp bl, 0
    jb invalid
    cmp bl, 10
    ja invalid

    ; Compute the factorial using a recursive procedure
    push dx
    push bx
    mov bx, bl
    call factorial

    ; Display the result
    mov ah, 9
    mov dx, offset prompt2
    int 21h

    mov dl, bl
    add dl, '0'
    mov ah, 2
    mov dx, offset result
    int 21h

    mov ah, 2
    mov dl, bl
    add dl, '0'
    int 21h

    mov ah, 4ch
    int 21h

invalid:
    mov ah, 9
    mov dx, offset newline
    int 21h
    mov ah, 4ch
    int 21h

factorial proc
    cmp bx, 1
    je base_case

    ; Recursive call
    push bx
    dec bx
    call factorial
    pop bx

    ; Multiply the result by the current value of bx
    mul bx

base_case:
    ret

endp factorial

endp main
```

Explanation:

* The code begins with the definition of the data segment (.data) and the code segment (.code).
* The data segment contains the following data:
    * prompt1: The prompt to display to the user to enter a non-negative integer.
    * prompt2: The prompt to display before the factorial result.
    * result: The string "is" to display after the factorial result.
    * newline: A newline character to terminate the output.
* The code segment contains the following procedures:
    * main: The main procedure that calls the factorial procedure to compute the factorial of a non-negative integer entered by the user.
    * factorial: A recursive procedure that computes the factorial of a non-negative integer.
* The main procedure begins by displaying the prompt to the user to enter a non-negative integer.
* The user enters a non-negative integer and presses Enter.
* The main procedure checks if the entered integer is valid (between 0 and 9). If it is invalid, an error message is displayed and the program terminates.
* If the entered integer is valid, the main procedure calls the factorial procedure to compute the factorial of the integer.
* The factorial procedure uses recursion to compute the factorial. It checks if the integer is 1. If it is, the factorial is 1. Otherwise, the procedure recursively calls itself with the integer minus 1 and multiplies the result by the current value of the integer.
* The factorial procedure returns the computed factorial to the main procedure.
* The main procedure displays the factorial result along with the appropriate prompts.
* The program terminates after displaying the factorial result.