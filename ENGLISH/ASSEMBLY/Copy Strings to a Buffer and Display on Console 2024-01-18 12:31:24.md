```
.model small
.stack 100h

.data
    string1 db 'Hello, world!'
    string2 db 'This is a test.'
    buffer db 64 dup (?)

.code
    mov ax, @data
    mov ds, ax

    mov ax, string1
    mov bx, buffer
    mov cx, 14
    rep movsb

    mov ax, string2
    add bx, 14
    mov cx, 14
    rep movsb

    mov ah, 9
    mov dx, buffer
    int 21h

    mov ah, 4ch
    int 21h

.end
```

Explanation:

* The code starts by defining a data segment and a stack segment.
* The data segment contains two strings, `string1` and `string2`, and a buffer `buffer` that is large enough to hold both strings.
* The code segment contains a series of instructions that copy the two strings into the buffer, and then display the buffer on the console.
* The first instruction, `mov ax, @data`, loads the address of the data segment into the AX register.
* The second instruction, `mov ds, ax`, sets the DS register to the value of AX, which makes the data segment the default segment for all subsequent instructions.
* The third instruction, `mov ax, string1`, loads the address of the first string into the AX register.
* The fourth instruction, `mov bx, buffer`, loads the address of the buffer into the BX register.
* The fifth instruction, `mov cx, 14`, loads the length of the first string (14 characters) into the CX register.
* The sixth instruction, `rep movsb`, copies the first string from the memory location pointed to by AX to the memory location pointed to by BX, repeating the operation CX times.
* The seventh instruction, `mov ax, string2`, loads the address of the second string into the AX register.
* The eighth instruction, `add bx, 14`, adds 14 to the value of BX, which moves the pointer to the end of the first string.
* The ninth instruction, `mov cx, 14`, loads the length of the second string (14 characters) into the CX register.
* The tenth instruction, `rep movsb`, copies the second string from the memory location pointed to by AX to the memory location pointed to by BX, repeating the operation CX times.
* The eleventh instruction, `mov ah, 9`, sets the AH register to 9, which is the DOS function code for printing a string.
* The twelfth instruction, `mov dx, buffer`, loads the address of the buffer into the DX register.
* The thirteenth instruction, `int 21h`, interrupts the operating system with the function code in AH, which causes the string in the buffer to be printed on the console.
* The fourteenth instruction, `mov ah, 4ch`, sets the AH register to 4ch, which is the DOS function code for exiting the program.
* The fifteenth instruction, `int 21h`, interrupts the operating system with the function code in AH, which causes the program to exit.