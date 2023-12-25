Claro! Aqui está um código complexo em Assembly x86 que realiza a ordenação de um vetor utilizando o algoritmo Quicksort. O código está comentado em português para facilitar o entendimento:

```
section .data
    vetor db 5, 10, 3, 8, 1, 7, 4, 2, 9, 6     ; Vetor a ser ordenado
    tamanho equ $ - vetor                     ; Tamanho do vetor

section .text
    global _start
_start:
    mov esi, vetor                           ; ESI aponta para o início do vetor
    mov ecx, tamanho                         ; ECX recebe o tamanho do vetor
    dec ecx                                  ; Decrementa o tamanho para obter o índice do último elemento

    push ecx                                 ; Salva o valor de ECX na pilha

    call quicksort                           ; Chama a função quicksort

    mov eax, 4                               ; Número da syscall write para imprimir o vetor
    mov ebx, 1                               ; Descritor de arquivo STDOUT
    mov ecx, vetor                           ; Ponteiro para o vetor
    mov edx, tamanho                         ; Tamanho do vetor
    int 0x80                                 ; Chama a syscall write

    mov eax, 1                               ; Número da syscall exit
    xor ebx, ebx                             ; Código de saída 0
    int 0x80                                 ; Chama a syscall exit

quicksort:
    pop ecx                                  ; Recupera o valor de ECX da pilha
    cmp ecx, 0                               ; Verifica se o tamanho do vetor é 0 ou 1
    jle fim_quicksort                         ; Se sim, retorna

    push esi                                 ; Salva o valor de ESI na pilha
    push ecx                                 ; Salva o valor de ECX na pilha

    mov eax, ecx                             ; EAX recebe o tamanho do vetor
    shr eax, 1                               ; Divide o tamanho por 2 para obter o índice do pivô
    imul eax, 1                              ; Multiplica o índice por 2 para obter o deslocamento em bytes
    add esi, eax                             ; ESI aponta para o pivô

    mov al, [esi]                            ; AL recebe o valor do pivô
    xor edi, edi                             ; EDI será utilizado como índice para a partição
    xor ebx, ebx                             ; EBX será utilizado como índice para a troca

particao:
    inc edi                                  ; Incrementa o índice da partição
    mov dl, [esi + edi]                      ; DL recebe o valor do elemento atual

    cmp dl, al                               ; Compara o valor do elemento atual com o valor do pivô
    jge nao_troca                            ; Se for maior ou igual, pula para a próxima iteração

    mov al, [esi + edi]                      ; Atualiza o valor do pivô com o valor do elemento atual

    mov bl, [esi + edi]                      ; BL recebe o valor do elemento atual
    xchg [esi + ebx], [esi + edi]            ; Troca os valores dos elementos

nao_troca:
    cmp edi, ecx                             ; Verifica se a partição chegou ao final
    jne particao                             ; Se não, continua a partição

    mov al, [esi + ebx]                      ; Atualiza o valor do pivô com o valor do último elemento da partição

    xchg [esi + ebx], [esi]                  ; Troca o valor do pivô com o último elemento da partição

    push ecx                                 ; Salva o tamanho da segunda partição na pilha
    sub ecx, ebx                             ; Atualiza o tamanho da primeira partição
    call quicksort                           ; Chama a função quicksort para a primeira partição
    pop ecx                                  ; Recupera o tamanho da segunda partição da pilha
    add esi, ebx                             ; Atualiza o ponteiro para a segunda partição
    dec ecx                                  ; Decrementa o tamanho da segunda partição
    call quicksort                           ; Chama a função quicksort para a segunda partição

fim_quicksort:
    ret                                      ; Retorna da função quicksort
```

Esse código utiliza o algoritmo Quicksort para ordenar um vetor de números inteiros em ordem crescente. A função `quicksort` é implementada de forma recursiva, dividindo o vetor em partições menores até que cada partição tenha tamanho zero ou um.

A função `quicksort` recebe como parâmetros o ponteiro para o vetor (`ESI`) e o tamanho do vetor (`ECX`). Em cada chamada recursiva, a função seleciona um pivô e particiona o vetor em elementos menores e maiores que o pivô. Em seguida, a função é chamada recursivamente para ordenar as duas partições separadamente.

Após a chamada da função `quicksort` no ponto de partida `_start`, o vetor é impresso na saída padrão utilizando a syscall `write` e o programa é finalizado com a syscall `exit`.

Espero que este código complexo e diferenciado em Assembly atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais detalhes, sinta-se à vontade para perguntar.