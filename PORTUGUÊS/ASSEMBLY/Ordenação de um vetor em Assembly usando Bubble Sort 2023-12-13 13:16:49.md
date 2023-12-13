Claro! Vou criar um código em Assembly que realiza a ordenação de um vetor de números usando o algoritmo Bubble Sort. O Bubble Sort é um algoritmo de ordenação simples, porém, não otimizado.

```assembly
section .data
    vetor db 5, 2, 7, 1, 9, 3, 6, 4, 8    ; vetor de números
    tamanho equ $ - vetor                ; tamanho do vetor

section .text
    global _start

_start:
    mov ecx, tamanho                    ; contador
    dec ecx                             ; decrementa uma vez para não comparar o último elemento

loop_externo:
    mov ebx, 0                          ; flag para verificar se houve troca
    mov edi, ecx                        ; salva o contador atual em edi

loop_interno:
    mov al, [vetor + ebx]               ; elemento atual
    mov dl, [vetor + ebx + 1]           ; próximo elemento

    cmp al, dl                          ; compara os elementos
    jle nao_troca                       ; se não for maior, não há troca
      
    mov [vetor + ebx], dl               ; troca os elementos
    mov [vetor + ebx + 1], al

    mov ebx, 1                          ; seta a flag para indicar que houve troca

nao_troca:
    inc ebx                             ; incrementa ebx para a próxima comparação
    loop loop_interno                    ; repete o loop até percorrer todo o vetor

    cmp ebx, 0                          ; se a flag for zero, significa que o vetor está ordenado
    jz fim

    dec edi                             ; decrementa o contador
    jnz loop_externo                    ; repete o loop externo se o contador for diferente de zero

fim:
    ; imprime o vetor ordenado
    mov ecx, tamanho
    mov ebx, 0

imprime_vetor:
    mov al, [vetor + ebx]
    add al, 30h                         ; converte o número para o correspondente caractere ASCII
    mov [vetor + ebx], al               ; substitui o número pelo caractere

    mov eax, 4                          ; chamada de sistema para imprimir
    mov ebx, 1                          ; arquivo de saída (stdout)
    mov edx, 1                          ; tamanho do dado a ser impresso (1 byte)
    int 0x80                            ; interrupção do sistema

    inc ebx                             ; incrementa ebx para o próximo elemento do vetor
    loop imprime_vetor

exit:
    mov eax, 1                          ; chamada de sistema para sair
    xor ebx, ebx                        ; código de saída
    int 0x80                            ; interrupção do sistema
```

Este código em Assembly realiza a ordenação do vetor de números usando o algoritmo Bubble Sort. A cada iteração, ele compara dois elementos adjacentes e os troca de posição se estiverem fora de ordem. O código continua fazendo iterações até que não haja mais trocas a serem feitas, o que significa que o vetor está ordenado.

Após a ordenação, o código imprime o vetor ordenado na saída padrão.

Lembrando que este código é apenas um exemplo e pode ser adaptado para diferentes necessidades.