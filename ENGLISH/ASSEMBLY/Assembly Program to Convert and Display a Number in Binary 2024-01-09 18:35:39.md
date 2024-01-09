```
.model small
.stack 100h

.data
msg1 db "Enter a number: $"
msg2 db "The number is: $"

.code
main proc
    mov ax, @data
    mov ds, ax

    mov ah, 9
    lea dx, msg1
    int 21h

    mov ah, 1
    int 21h
    sub al, '0'
    mov bh, al

    mov ah, 2
    mov dl, bh
    int 21h

    mov ah, 9
    lea dx, msg2
    int 21h

    mov ah, 2
    mov dl, bh
    int 21h

    mov ah, 4ch
    int 21h

main endp
end main
```

This program prompts the user to enter a number, reads the input, converts it to a binary number, and displays the result.

The following is a brief explanation of the code:

* The `.model small` directive specifies that the program will be a small model program, which means that it will use a single 16-bit segment for both code and data.
* The `.stack 100h` directive specifies that the program will have a stack size of 100 bytes.
* The `.data` segment is used to store the program's data, including the messages that are displayed to the user.
* The `.code` segment is used to store the program's code.
* The `main` procedure is the entry point of the program.
* The `mov ax, @data` instruction moves the address of the `.data` segment into the AX register.
* The `mov ds, ax` instruction sets the DS register to the address of the `.data` segment, which makes the data in the `.data` segment accessible to the program.
* The `mov ah, 9` instruction sets the AH register to 9, which is the DOS function code for printing a string.
* The `lea dx, msg1` instruction loads the address of the `msg1` string into the DX register.
* The `int 21h` instruction calls the DOS interrupt 21h, which prints the string that is stored at the address in the DX register.
* The `mov ah, 1` instruction sets the AH register to 1, which is the DOS function code for reading a character from the keyboard.
* The `int 21h` instruction calls the DOS interrupt 21h, which reads a character from the keyboard and stores it in the AL register.
* The `sub al, '0'` instruction subtracts the ASCII code for '0' from the AL register, which converts the ASCII character to a numeric value.
* The `mov bh, al` instruction moves the numeric value from the AL register to the BH register.
* The `mov ah, 2` instruction sets the AH register to 2, which is the DOS function code for displaying a character.
* The `mov dl, bh` instruction moves the numeric value from the BH register to the DL register.
* The `int 21h` instruction calls the DOS interrupt 21h, which displays the character that is stored in the DL register.
* The `mov ah, 9` instruction sets the AH register to 9, which is the DOS function code for printing a string.
* The `lea dx, msg2` instruction loads the address of the `msg2` string into the DX register.
* The `int 21h` instruction calls the DOS interrupt 21h, which prints the string that is stored at the address in the DX register.
* The `mov ah, 2` instruction sets the AH register to 2, which is the DOS function code for displaying a character.
* The `mov dl, bh` instruction moves the numeric value from the BH register to the DL register.
* The `int 21h` instruction calls the DOS interrupt 21h, which displays the character that is stored in the DL register.
* The `mov ah, 4ch` instruction sets the AH register to 4ch, which is the DOS function code for exiting the program.
* The `int 21h` instruction calls the DOS interrupt 21h, which exits the program.