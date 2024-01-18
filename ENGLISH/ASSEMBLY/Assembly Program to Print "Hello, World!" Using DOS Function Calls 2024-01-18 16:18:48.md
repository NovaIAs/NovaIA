```
.MODEL SMALL
.STACK 100H
.DATA
MESSAGE DB 'HELLO, WORLD!'
MESSAGE_LENGTH EQU $-MESSAGE  ; Length of the message string
.CODE
MAIN PROC
    MOV AX, @DATA  ; Set DS to the data segment
    MOV DS, AX

    MOV AH, 9      ; DOS function for printing a string
    MOV DX, OFFSET MESSAGE  ; Address of the message string
    INT 21H        ; Call DOS to print the string

    MOV AH, 4CH   ; DOS function for exiting the program
    INT 21H        ; Call DOS to exit the program
MAIN ENDP
END MAIN
```

This program prints the message "HELLO, WORLD!" to the console using DOS function calls. The code is explained below:

* The `.MODEL SMALL` directive specifies that the program is a small model program, which means that it can only access the first 64KB of memory.
* The `.STACK 100H` directive specifies that the program will use a stack size of 100 hexadecimal bytes (256 bytes).
* The `.DATA` directive starts the data segment, which is used to store data such as strings and variables.
* The `MESSAGE` directive defines a string constant that contains the message "HELLO, WORLD!".
* The `MESSAGE_LENGTH` directive defines an equ (constant) that contains the length of the message string.
* The `.CODE` directive starts the code segment, which is used to store the program's instructions.
* The `MAIN` procedure is the entry point of the program.
* The `MOV AX, @DATA` instruction sets the DS register to the address of the data segment. This allows the program to access data in the data segment.
* The `MOV DS, AX` instruction sets the DS register to the value of the AX register. This makes the data segment the default segment for all data accesses.
* The `MOV AH, 9` instruction sets the AH register to 9, which is the DOS function number for printing a string.
* The `MOV DX, OFFSET MESSAGE` instruction sets the DX register to the address of the message string.
* The `INT 21H` instruction calls DOS to print the string.
* The `MOV AH, 4CH` instruction sets the AH register to 4CH, which is the DOS function number for exiting the program.
* The `INT 21H` instruction calls DOS to exit the program.
* The `MAIN ENDP` directive ends the `MAIN` procedure.
* The `END MAIN` directive ends the program.