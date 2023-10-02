Claro! Aqui está um exemplo de código em Assembly, que implementa um algoritmo de ordenação chamado Bubble Sort:

```assembly
section .data
    array db 5, 2, 8, 3, 1         ; array a ser ordenado
    array_size equ $ - array       ; tamanho do array

section .text
    global _start

_start:
    mov ecx, 0                    ; contador externo
    
outer_loop:
    mov eax, 0                    ; contador interno
    mov ebx, ecx                  ; valor atual do contador externo
    add ebx, 1                    ; próximo valor do contador externo
    
inner_loop:
    mov edx, array_size            ; tamanho do array
    sub edx, ecx                   ; subtrai contador externo do tamanho do array
    sub edx, 1                     ; subtrai 1 do tamanho do array
    
    cmp eax, edx                   ; compara contador interno com tamanho do array - contador externo - 1
    jge next_outer_loop            ; se contador interno for maior ou igual, sai do loop interno
    
    mov esi, eax                   ; valor atual do contador interno
    add esi, ecx                   ; adiciona contador externo ao contador interno
    
    mov edi, esi                   ; posição atual do array
    
    mov bl, [array + esi]          ; valor atual do array
    cmp bl, [array + ebx]          ; compara valor atual do array com próximo valor do array
    jle no_swap                    ; se valor atual do array for menor ou igual, não realiza a troca
    
    mov dl, [array + ebx]          ; próximo valor do array
    mov [array + esi], dl          ; troca os valores no array
    mov [array + ebx], bl
    
no_swap:
    add eax, 1                     ; incrementa contador interno
    jmp inner_loop                 ; volta para o início do loop interno

next_outer_loop:
    add ecx, 1                     ; incrementa contador externo
    cmp ecx, array_size            ; compara contador externo com tamanho do array
    jl outer_loop                  ; se contador externo for menor, volta para o início do loop externo

exit:
    mov eax, 1                     ; número de sistema para exit
    xor ebx, ebx                   ; código de saída (0)
    int 0x80                       ; chama a interrupção do sistema para finalizar o programa
```

Espero que goste! Esse código implementa o algoritmo Bubble Sort em Assembly, que realiza a ordenação de um array de números.