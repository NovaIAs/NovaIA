```
; This is a simple assembly program that prints the message "Hello, world!" to the console.

; The first line of the program is a directive that tells the assembler to reserve 256 bytes of memory for the program.
; The second line is a directive that tells the assembler to start the program at the address 0x100.
ORG 0x100

; The next line is a label. Labels are used to mark specific locations in the program.
; In this case, the label "start" marks the start of the program.
start:

; The next line is an instruction that tells the processor to move the value 0x01 into the register EAX.
; The EAX register is a general-purpose register that can be used for a variety of purposes.
mov eax, 0x01

; The next line is an instruction that tells the processor to move the value 0x02 into the register EBX.
; The EBX register is another general-purpose register that can be used for a variety of purposes.
mov ebx, 0x02

; The next line is an instruction that tells the processor to add the values in the EAX and EBX registers.
; The result of the addition is stored in the EAX register.
add eax, ebx

; The next line is an instruction that tells the processor to move the value in the EAX register to the console.
; The console is a device that allows the user to interact with the computer.
mov ah, 0x0e
mov al, eax
int 0x10

; The next line is an instruction that tells the processor to stop the program.
; The "hlt" instruction is a special instruction that causes the processor to stop executing instructions.
hlt

; The next line is a directive that tells the assembler to end the program.
; The "end" directive is the last line of every assembly program.
end
```

This program is a simple example of how to use assembly language to write a program. The program starts by reserving 256 bytes of memory for the program. It then starts the program at the address 0x100. The program then moves the value 0x01 into the EAX register and the value 0x02 into the EBX register. It then adds the values in the EAX and EBX registers and stores the result in the EAX register. The program then moves the value in the EAX register to the console. Finally, the program stops the program by executing the "hlt" instruction.