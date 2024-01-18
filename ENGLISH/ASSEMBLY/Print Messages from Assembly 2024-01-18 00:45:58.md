```assembly
.model medium
.data
message1: .asciiz "Hello, world!\n"
message2: .asciiz "This is a message from assembly.\n"
.code
start:
    mov ax, @data		; Load the address of the data segment into AX
    mov ds, ax			; Set DS to point to the data segment
    
    mov ah, 09h			; Set AH to the interrupt for printing a string
    mov dx, offset message1	; Load the offset of the first message into DX
    int 21h			; Call the interrupt to print the first message
    
    mov ah, 09h			; Set AH to the interrupt for printing a string
    mov dx, offset message2	; Load the offset of the second message into DX
    int 21h			; Call the interrupt to print the second message
    
    mov ah, 4Ch			; Set AH to the interrupt for exiting the program
    int 21h			; Call the interrupt to exit the program
```

This code is a simple program in assembly language that prints two messages to the screen and then exits. The program starts with the start label, which is the entry point of the program. The first few lines of the program load the address of the data segment into the AX register and then set the DS register to point to the data segment. This is necessary because the program needs to access the data segment in order to print the messages.

The next few lines of the program set the AH register to the interrupt for printing a string and then load the offset of the first message into the DX register. The int 21h instruction is then called to print the first message. The same process is repeated for the second message.

Finally, the program sets the AH register to the interrupt for exiting the program and then calls the int 21h instruction to exit the program.

This code is a bit more complex than the previous one, but it is still relatively simple. It demonstrates how to load data from the data segment, print messages to the screen, and exit the program.