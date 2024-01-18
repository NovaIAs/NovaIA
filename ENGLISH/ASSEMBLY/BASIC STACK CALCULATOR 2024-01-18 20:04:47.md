```
; This program is a simple calculator that can perform addition, subtraction, multiplication, and division.
; It uses the stack to store the operands and the results of the operations.

; The stack is a data structure that grows and shrinks dynamically.
; It is implemented using an array of memory locations.
; The top of the stack is the last element that was added to the stack.
; The bottom of the stack is the first element that was added to the stack.

; The stack pointer is a register that points to the top of the stack.
; When the stack is empty, the stack pointer points to the bottom of the stack.
; When the stack is full, the stack pointer points to the last element in the stack.

; The following code defines the stack data structure.

; The stack array is 100 bytes long.
STACK_SIZE EQU 100

; The stack pointer points to the top of the stack.
STACK_POINTER EQU 0

; The following code defines the four arithmetic operations.

; The add operation adds the top two elements of the stack and pushes the result onto the stack.
ADD:
    POP AX, [STACK_POINTER]
    POP BX, [STACK_POINTER]
    ADD AX, BX
    PUSH AX, [STACK_POINTER]
    RET

; The subtract operation subtracts the top two elements of the stack and pushes the result onto the stack.
SUB:
    POP AX, [STACK_POINTER]
    POP BX, [STACK_POINTER]
    SUB AX, BX
    PUSH AX, [STACK_POINTER]
    RET

; The multiply operation multiplies the top two elements of the stack and pushes the result onto the stack.
MUL:
    POP AX, [STACK_POINTER]
    POP BX, [STACK_POINTER]
    MUL AX, BX
    PUSH AX, [STACK_POINTER]
    RET

; The divide operation divides the top two elements of the stack and pushes the result onto the stack.
DIV:
    POP AX, [STACK_POINTER]
    POP BX, [STACK_POINTER]
    DIV AX, BX
    PUSH AX, [STACK_POINTER]
    RET

; The following code defines the main program.

; The main program reads a line of input from the user.
; The input line contains the operands and the operation to be performed.
; The main program parses the input line and calls the appropriate arithmetic operation.
; The result of the operation is printed to the screen.

MAIN:
    ; Read a line of input from the user.
    MOV AH, 01
    MOV DX, OFFSET INPUT_BUFFER
    INT 21h

    ; Parse the input line.
    CALL PARSE_INPUT

    ; Call the appropriate arithmetic operation.
    CMP OPERATION, '+'
    JE ADD
    CMP OPERATION, '-'
    JE SUB
    CMP OPERATION, '*'
    JE MUL
    CMP OPERATION, '/'
    JE DIV

    ; Print the result of the operation to the screen.
    POP AX, [STACK_POINTER]
    MOV AH, 02
    MOV DX, AX
    INT 21h

    ; Exit the program.
    MOV AH, 4Ch
    INT 21h

; The following code defines the input buffer.

; The input buffer is 80 bytes long.
INPUT_BUFFER_SIZE EQU 80

; The input buffer is located at the bottom of the stack.
INPUT_BUFFER: TIMES INPUT_BUFFER_SIZE DB 0

; The following code defines the parse input subroutine.

; The parse input subroutine parses the input line and stores the operands and the operation to be performed in the appropriate variables.

PARSE_INPUT:
    ; Find the first operand.
    MOV DI, OFFSET INPUT_BUFFER
    CALL FIND_OPERAND
    MOV OPERAND1, EAX

    ; Find the operation.
    CALL FIND_OPERATION
    MOV OPERATION, AL

    ; Find the second operand.
    CALL FIND_OPERAND
    MOV OPERAND2, EAX

    ; Return to the main program.
    RET

; The following code defines the find operand subroutine.

; The find operand subroutine finds the first operand in the input line and returns it in the EAX register.

FIND_OPERAND:
    ; Find the first non-whitespace character.
    MOV CX, 0
    MOVZX ECX, WORD PTR [DI]
    CMP ECX, SPACE
    JE FIND_OPERAND
    INC DI

    ; Read the operand.
    MOV EAX, 0
    MOVZX ECX, WORD PTR [DI]
    CMP ECX, SPACE
    JNE FIND_OPERAND
    DEC DI

    ; Return the operand.
    RET

; The following code defines the find operation subroutine.

; The find operation subroutine finds the operation in the input line and returns it in the AL register.

FIND_OPERATION:
    ; Find the first non-whitespace character.
    MOV CX, 0
    MOVZX ECX, WORD PTR [DI]
    CMP ECX, SPACE
    JE FIND_OPERATION
    INC DI

    ; Read the operation.
    MOV AL, [DI]
    INC DI

    ; Return the operation.
    RET
```