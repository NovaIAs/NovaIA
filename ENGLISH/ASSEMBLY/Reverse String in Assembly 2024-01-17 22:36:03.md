```
section .data
  ; Define the input and output strings
  input_string: db "Enter a string: ", 10
  output_string: db "The reversed string is: ", 10, 0

section .text
  ; Main program entry point
  global main

main:
  ; Print the input prompt
  mov eax, 4
  mov ebx, 1
  mov ecx, input_string
  mov edx, strlen(input_string)
  int 80h

  ; Read the input string from the user
  mov eax, 3
  mov ebx, 0
  mov ecx, buffer
  mov edx, 255
  int 80h

  ; Reverse the input string
  call reverse_string
  add esp, 4 ; Adjust the stack pointer to pop the return address

  ; Print the reversed string
  mov eax, 4
  mov ebx, 1
  mov ecx, reversed_buffer
  mov edx, strlen(reversed_buffer)
  int 80h

  ; Print a newline
  mov eax, 4
  mov ebx, 1
  mov ecx, output_string
  mov edx, strlen(output_string)
  int 80h

  ; Exit the program
  mov eax, 1
  mov ebx, 0
  int 80h

; Subroutine to reverse the input string
reverse_string:
  ; Save the base address of the input string in ESI
  push ebx
  push esi
  push edi
  mov esi, ecx

  ; Calculate the length of the input string
  mov edi, 0
  mov bl, [esi]
  cmp bl, 0
  je find_null
inc_loop:
  inc edi
  add esi, 1
  mov bl, [esi]
  cmp bl, 0
  jne inc_loop

find_null:

  ; Initialize index variables
  mov ebx, 0
  mov edi, edi

sub_loop:

  ; Stop when index is equal to length of string
  cmp ebx, edi
  je end_loop

  ; Swap characters at current index and length - index - 1
  mov bl, [esi + ebx]
  mov dl, [esi + edi - ebx - 1]
  mov [esi + ebx], dl
  mov [esi + edi - ebx - 1], bl

  ; Increment index
  inc ebx
  jmp sub_loop

end_loop:
  mov ecx, reversed_buffer
  mov eax, edi
  mov [ecx], al
  add ecx, 1
  mov al, 0
  mov [ecx], al

  ; Restore registers and return
  pop edi
  pop esi
  pop ebx
  ret

; Function to calculate the length of a string
strlen:
  push ebx
  mov ebx, ecx
  mov al, [ebx]
  cmp al, 0
  jz done
loop_strlen:
  inc ebx
  mov al, [ebx]
  cmp al, 0
  jnz loop_strlen
done:
  sub ebx, ecx
  pop ebx
  ret

; Data segment definitions
section .data
  ; Define a buffer to store the input string
  buffer: times 256 db 0

  ; Define a buffer to store the reversed string
  reversed_buffer: times 256 db 0
```

This code is a complete program that prompts the user to enter a string, reverses the string, and then prints the reversed string to the console. The program uses a subroutine called `reverse_string` to reverse the input string. The `reverse_string` subroutine uses a loop to swap the characters at the beginning and end of the string until the middle of the string is reached.

The program also uses a function called `strlen` to calculate the length of the input string. The `strlen` function uses a loop to count the number of characters in the string until it reaches a null character (ASCII code 0).

The program is written in 32-bit assembly language and uses the x86 calling conventions. The program is divided into three sections: the data segment, the text segment, and the bss segment. The data segment contains the string definitions and the buffer for the input string. The text segment contains the main program code and the `reverse_string` and `strlen` functions. The bss segment is used to store the reversed string buffer.

To run the program, you can assemble it using an assembler such as NASM or TASM and then link it with a linker such as LD. Once the program is linked, you can run it by typing the program name at the command prompt.