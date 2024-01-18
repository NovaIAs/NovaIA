```assembly
    .include "DOS.INC"
    .model small
    .stack 100h

    .data
    string1: db "Hello, world!", 10
    string2: db "This is a test", 10
    string3: db "This is another test", 10
    string4: db "This is a very long test", 10

    .code
    start:
        mov ax, @data
        mov ds, ax

        ; Print string1
        mov dx, offset string1
        call print

        ; Print string2
        mov dx, offset string2
        call print

        ; Print string3
        mov dx, offset string3
        call print

        ; Print string4
        mov dx, offset string4
        call print

        ; Exit the program
        mov ax, 4C00h
        int 21h

    print:
        ; Save the registers
        push ax
        push bx
        push cx
        push dx
        push si
        push di

        ; Set up the print function
        mov ah, 9
        mov dx, [bx]
        int 21h

        ; Restore the registers
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax

        ; Return from the print function
        ret

    end start
```

This code is a simple program that prints four strings to the console. It uses the DOS interrupt 9 to print the strings.

The code starts by defining the strings in the .data section. The strings are defined as follows:

```
string1: db "Hello, world!", 10
string2: db "This is a test", 10
string3: db "This is another test", 10
string4: db "This is a very long test", 10
```

The db directive is used to define a byte array. The first byte in the array is the length of the string. The remaining bytes are the characters in the string. The 10 at the end of each string is the ASCII code for the newline character.

The code then defines the start label, which is the entry point for the program. The start label is followed by a series of instructions that perform the following tasks:

1. Move the address of the .data segment into the AX register.
2. Move the AX register into the DS register. This sets the data segment register to point to the .data segment.
3. Move the offset of string1 into the DX register.
4. Call the print function.
5. Repeat steps 3 and 4 for strings 2, 3, and 4.
6. Exit the program by calling the DOS interrupt 4C00h.

The print function is defined as follows:

```
print:
    ; Save the registers
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    ; Set up the print function
    mov ah, 9
    mov dx, [bx]
    int 21h

    ; Restore the registers
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    ; Return from the print function
    ret
```

The print function starts by saving the values of the AX, BX, CX, DX, SI, and DI registers on the stack. This is done to preserve the values of these registers while the function is running.

The function then sets up the print function by moving the value 9 into the AH register. This tells DOS to perform the print function. The function then moves the offset of the string to be printed into the DX register.

The function then calls the DOS interrupt 21h. This causes DOS to print the string to the console.

After the string has been printed, the function restores the values of the AX, BX, CX, DX, SI, and DI registers from the stack. The function then returns.