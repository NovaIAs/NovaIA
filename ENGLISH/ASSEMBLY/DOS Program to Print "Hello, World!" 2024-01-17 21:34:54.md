```assembly
BITS 16          ; 16-bit addressing
ORG 100H         ; Start of program at 100H

; Define memory locations for variables
DATA_PTR DW 0     ; Pointer to data
DATA_SIZE DW 0     ; Size of data

; Define code segments
CODE_SEGMENT SEGMENT
__START:
  MOV AX, @DATA   ; Load address of data into AX
  MOV DS, AX      ; Set DS to data segment
  MOV ES, AX      ; Set ES to data segment

  MOV DI, 0       ; Initialize DI to 0
  MOV CX, [DATA_SIZE] ; Load data size into CX

  ; Loop through data and print each byte
  _LOOP:
    MOV AL, [DI]    ; Load byte from data into AL
    CALL PRINT_BYTE ; Call subroutine to print byte
    INC DI          ; Increment DI to next byte
    LOOP _LOOP      ; Loop until CX reaches 0

; Subroutine to print a byte
PRINT_BYTE PROC
  MOV AH, 02H      ; DOS function 2 - Write Character
  INT 21H         ; Call DOS
  RET             ; Return from subroutine
PRINT_BYTE ENDP

; End of code segment
CODE_SEGMENT ENDS

; Data segment
DATA_SEGMENT SEGMENT
DATA_PTR DD DATA
DATA:
  DB 'Hello, World!'  ; Data to be printed
DATA_SIZE DW $-DATA  ; Calculate size of data

; End of data segment
DATA_SEGMENT ENDS

; End of program
END __START
```

**Explanation:**

This program is a simple DOS program that prints the string "Hello, World!" to the console. It consists of three segments:

* **CODE_SEGMENT:** This segment contains the code for the program.
* **DATA_SEGMENT:** This segment contains the data for the program.
* **STACK_SEGMENT:** This segment is used for storing temporary data during the execution of the program.

The program starts at the __START label. The first thing the program does is to load the address of the data segment into the DS register. This tells the program where to find the data that it needs.

The program then enters a loop that prints each byte of the data to the console. The loop is controlled by the CX register, which contains the size of the data. The DI register is used to keep track of the current position in the data.

When the loop is finished, the program returns to the __START label and the program terminates.

The PRINT_BYTE procedure is a subroutine that is called to print a single byte to the console. The procedure takes the byte to be printed in the AL register and calls the DOS function 2 to write the byte to the console.

This program is a simple example of how to write a DOS program in assembly language.