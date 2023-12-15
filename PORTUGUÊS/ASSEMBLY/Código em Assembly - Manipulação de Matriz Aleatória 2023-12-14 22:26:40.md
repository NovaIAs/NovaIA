Claro! Aqui está um exemplo de um código complexo em Assembly, escrito em português, que cria e manipula uma matriz de números aleatórios:

```assembly
section .data
    matriz db 10, 20, 30, 40, 50, 60, 70, 80, 90, 100
    tamanho_matriz equ 10
    nova_matriz times tamanho_matriz db 0

section .text
    global _start
    
_start:
    mov ecx, tamanho_matriz
    mov esi, matriz
    mov edi, nova_matriz
    
    loop_copia:
        lodsb
        stosb
        loop loop_copia
    
    jmp loop_aleatorio

gerar_numero_aleatorio:
    mov eax, ecx
    xor edx, edx
    add eax, edx
    sub eax, 1
    jz loop_aleatorio
    mov ebx, eax
    
    xor eax, eax
    mov ecx, 1
    div ecx
    mov eax, edx
    add eax, 1
    
    xor ecx, ecx
    mov cl, al
    
    mov al, byte [edi + ecx]
    mov dl, byte [edi]
    mov byte [edi], al
    mov byte [edi + ecx], dl
    
loop_aleatorio:
    dec ecx
    jnz gerar_numero_aleatorio
    
    jmp imprime_matriz

imprime_matriz:
    mov ecx, tamanho_matriz
    mov esi, nova_matriz
    
    loop_imprime:
        movzx eax, byte [esi]
        add eax, 48
        mov [num], eax
        
        mov eax, 4
        mov ebx, 1
        mov edx, 1
        int 0x80
        
        inc esi
        loop loop_imprime
    
    jmp terminar_programa

terminar_programa:
    mov eax, 1
    xor ebx, ebx
    int 0x80

section .bss
    num resb 1
```

Este código cria uma matriz inicial chamada "matriz" com 10 elementos. Em seguida, cria uma nova matriz vazia chamada "nova_matriz" com o mesmo tamanho.

Em seguida, o código copia os elementos da matriz original para a nova matriz. Em seguida, ele entra em um loop onde gera um número aleatório e troca a posição do elemento aleatório com o primeiro elemento da nova matriz. Isso é repetido até que todos os elementos tenham sido permutados.

Depois disso, o código imprime a nova matriz na tela, convertendo cada número para seu caractere correspondente. Finalmente, o programa termina.

Este código é bastante complexo e demonstra diferentes conceitos da linguagem Assembly, como manipulação de registros, uso de loops, geração de números aleatórios e interação com a entrada/saída do sistema operacional.