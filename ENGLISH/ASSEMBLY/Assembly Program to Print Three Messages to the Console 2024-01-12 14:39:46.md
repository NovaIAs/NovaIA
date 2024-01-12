```
.MODEL SMALL
.STACK 100H
.DATA
        MESSAGE1 DB 'HELLO, WORLD!', 13, 10, '$'
        MESSAGE2 DB 'THIS IS A VERY LONG MESSAGE THAT WILL BE PRINTED ON MULTIPLE LINES.', 13, 10, '$'
        MESSAGE3 DB 'AND THIS IS THE LAST MESSAGE.', 13, 10, '$'
```
This code is a simple program that prints three messages to the console. The messages are stored in the data segment of the program, and the program uses the DOS interrupt 21h to print them.

```
.CODE
        MAIN PROC
                MOV AX, @DATA
                MOV DS, AX

                MOV AH, 9
                MOV DX, OFFSET MESSAGE1
                INT 21H

                MOV AH, 9
                MOV DX, OFFSET MESSAGE2
                INT 21H

                MOV AH, 9
                MOV DX, OFFSET MESSAGE3
                INT 21H

                MOV AH, 4CH
                INT 21H
        MAIN ENDP
```
The main procedure of the program is responsible for printing the messages. It first moves the address of the data segment into the DS register, so that the program can access the data in the data segment.

Next, the program uses the DOS interrupt 21h to print the first message. The AH register is set to 9, which is the code for the "print string" interrupt. The DX register is set to the offset of the message in the data segment. The program then calls the interrupt, and the message is printed to the console.

The program then repeats this process for the second and third messages.

Finally, the program uses the DOS interrupt 4CH to exit the program.

```
END MAIN
```
The end of the program is marked by the END MAIN directive. This directive tells the assembler that the program is finished, and that it should generate the machine code for the program.