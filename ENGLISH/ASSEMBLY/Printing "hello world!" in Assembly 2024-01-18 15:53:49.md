```assembly
; This program prints "hello world!" on the console

; The first line is a comment
; Comments are ignored by the assembler

; The second line is a directive
; Directives tell the assembler what to do
; This directive tells the assembler to reserve 100 bytes of memory
.data
message: .asciz "hello world!"
; The .asciz directive creates a string
; The string is stored in memory

; The third line is a directive
; This directive tells the assembler where to start executing the program
.text
main:
; The main function is the entry point of the program

; The next line is an instruction
; Instructions tell the processor what to do
; This instruction loads the address of the message string into the eax register
mov eax, message

; The next line is an instruction
; This instruction prints the string in the eax register on the console
call print

; The next line is an instruction
; This instruction exits the program
; The exit code is 0, which indicates that the program ran successfully
mov eax, 0
call exit

; The next two lines are functions
; Functions are blocks of code that can be called from other parts of the program

print:
; The print function prints a string on the console

; The next line is an instruction
; This instruction pushes the eax register onto the stack
; The stack is a region of memory that is used to store data temporarily
push eax

; The next line is an instruction
; This instruction calls the print_string function
; The print_string function prints a string on the console
call print_string

; The next line is an instruction
; This instruction pops the eax register from the stack
pop eax

; The next line is an instruction
; This instruction returns from the print function
ret

print_string:
; The print_string function prints a string on the console

; The next line is an instruction
; This instruction gets the length of the string in the eax register
mov eax, [esp+4]

; The next line is an instruction
; This instruction calls the sys_write function
; The sys_write function writes data to the console
call sys_write

; The next line is an instruction
; This instruction returns from the print_string function
ret

; The next two lines are system calls
; System calls are functions that are provided by the operating system

sys_write:
; The sys_write function writes data to the console

; The next line is an instruction
; This instruction pushes the eax register onto the stack
push eax

; The next line is an instruction
; This instruction pushes the ebx register onto the stack
push ebx

; The next line is an instruction
; This instruction pushes the ecx register onto the stack
push ecx

; The next line is an instruction
; This instruction pushes the edx register onto the stack
push edx

; The next line is an instruction
; This instruction pushes the esi register onto the stack
push esi

; The next line is an instruction
; This instruction pushes the edi register onto the stack
push edi

; The next line is an instruction
; This instruction pushes the ebp register onto the stack
push ebp

; The next line is an instruction
; This instruction pushes the esp register onto the stack
push esp

; The next line is an instruction
; This instruction pushes the eflags register onto the stack
push eflags

; The next line is an instruction
; This instruction moves the eax register to the eax register
; This is a no-op instruction, but it is necessary to preserve the eax register
mov eax, eax

; The next line is an instruction
; This instruction moves the ebx register to the ebx register
; This is a no-op instruction, but it is necessary to preserve the ebx register
mov ebx, ebx

; The next line is an instruction
; This instruction moves the ecx register to the ecx register
; This is a no-op instruction, but it is necessary to preserve the ecx register
mov ecx, ecx

; The next line is an instruction
; This instruction moves the edx register to the edx register
; This is a no-op instruction, but it is necessary to preserve the edx register
mov edx, edx

; The next line is an instruction
; This instruction moves the esi register to the esi register
; This is a no-op instruction, but it is necessary to preserve the esi register
mov esi, esi

; The next line is an instruction
; This instruction moves the edi register to the edi register
; This is a no-op instruction, but it is necessary to preserve the edi register
mov edi, edi

; The next line is an instruction
; This instruction moves the ebp register to the ebp register
; This is a no-op instruction, but it is necessary to preserve the ebp register
mov ebp, ebp

; The next line is an instruction
; This instruction moves the esp register to the esp register
; This is a no-op instruction, but it is necessary to preserve the esp register
mov esp, esp

; The next line is an instruction
; This instruction moves the eflags register to the eflags register
; This is a no-op instruction, but it is necessary to preserve the eflags register
mov eflags, eflags

; The next line is an instruction
; This instruction calls the sys_write system call
call sys_write

; The next line is an instruction
; This instruction pops the eflags register from the stack
pop eflags

; The next line is an instruction
; This instruction pops the esp register from the stack
pop esp

; The next line is an instruction
; This instruction pops the ebp register from the stack
pop ebp

; The next line is an instruction
; This instruction pops the edi register from the stack
pop edi

; The next line is an instruction
; This instruction pops the esi register from the stack
pop esi

; The next line is an instruction
; This instruction pops the edx register from the stack
pop edx

; The next line is an instruction
; This instruction pops the ecx register from the stack
pop ecx

; The next line is an instruction
; This instruction pops the ebx register from the stack
pop ebx

; The next line is an instruction
; This instruction pops the eax register from the stack
pop eax

; The next line is an instruction
; This instruction returns from the sys_write system call
ret

exit:
; The exit system call exits the program

; The next line is an instruction
; This instruction pushes the eax register onto the stack
push eax

; The next line is an instruction
; This instruction pushes the ebx register onto the stack
push ebx

; The next line is an instruction
; This instruction pushes the ecx register onto the stack
push ecx

; The next line is an instruction
; This instruction pushes the edx register onto the stack
push edx

; The next line is an instruction
; This instruction pushes the esi register onto the stack
push esi

; The next line is an instruction
; This instruction pushes the edi register onto the stack
push edi

; The next line is an instruction
; This instruction pushes the ebp register onto the stack
push ebp

; The next line is an instruction
; This instruction pushes the esp register onto the stack
push esp

; The next line is an instruction
; This instruction pushes the eflags register onto the stack
push eflags

; The next line is an instruction
; This instruction moves the eax register to the eax register
; This is a no-op instruction, but it is necessary to preserve the eax register
mov eax, eax

; The next line is an instruction
;