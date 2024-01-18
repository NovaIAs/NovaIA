```
.data
; Define the number of elements in the array
num_elements = 10

; Define the array of 10 integers
array: .int 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

; Define a message to print
message: .asciiz "The array contains:"

.text
; Main program
main:
    ; Print the message
    movq $message, %rdi
    call printf

    ; Loop through the array and print each element
    xor %rcx, %rcx                 ; Initialize loop counter
loop:
    cmp %rcx, num_elements         ; Check if we've reached the end of the array
    je end_loop                     ; If so, jump to the end of the loop

    ; Print the current element
    movq array(%rcx, 4), %rdi      ; Get the address of the current element
    call printf

    ; Increment the loop counter
    inc %rcx
    jmp loop                        ; Jump back to the start of the loop

end_loop:
    ; Exit the program
    xor %eax, %eax
    ret

; Subroutine to print an integer
printf:
    ; Save the registers we'll be using
    push %rbp
    push %rbx

    ; Set up the stack for the printf call
    movq %rdi, %rbx
    xor %rdi, %rdi
    mov $1, %rax
    syscall

    ; Restore the registers we saved
    pop %rbx
    pop %rbp

    ; Return to the caller
    ret
```

This program defines an array of 10 integers, prints a message to the console, and then loops through the array and prints each element. The program uses the `printf` subroutine to print the message and the elements of the array. The `printf` subroutine takes a format string as its first argument and a variable number of arguments that are to be formatted according to the format string. In this case, the format string is "%d", which tells `printf` to print an integer.

The program uses the `syscall` instruction to call the `printf` subroutine. The `syscall` instruction takes a system call number as its first argument and a variable number of arguments that are passed to the system call. In this case, the system call number is 1, which is the number for the `printf` system call.

The program also uses the `xor` instruction to set the loop counter to 0 and to increment the loop counter at the end of each iteration of the loop. The `cmp` instruction is used to compare the loop counter to the number of elements in the array. If the loop counter is equal to the number of elements in the array, the program jumps to the end of the loop.

The `movq` instruction is used to move the address of the current element of the array into the `rdi` register. The `rdi` register is the first argument register for the `printf` subroutine. The `call` instruction is used to call the `printf` subroutine.

The `end_loop` label marks the end of the loop. The program uses the `jmp` instruction to jump to the end of the loop if the loop counter is equal to the number of elements in the array.

The `xor` instruction is used to set the return value of the program to 0. The `ret` instruction is used to return to the operating system.