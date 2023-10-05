Aqui está um código em assembly que realiza a ordenação de um vetor usando o algoritmo Bubble Sort:

```assembly
section .data
    vetor db 10, 5, 8, 2, 4        ; vetor de teste
    tamanho equ $ - vetor          ; tamanho do vetor

section .text
    global _start

_start:
    mov ecx, tamanho              ; quantidade de elementos no vetor
    dec ecx                       ; decrementa para ajustar o índice final

loop_outer:
    mov esi, vetor                ; carrega o endereço inicial do vetor
    xor edx, edx                  ; limpa o contador de trocas

loop_inner:
    mov al, [esi]                 ; carrega o elemento atual
    cmp al, [esi + 1]             ; compara com o próximo elemento
    jle no_swap                   ; se não for maior, pula para no_swap

    mov bl, [esi + 1]             ; salva o próximo elemento em bl
    mov [esi + 1], al             ; coloca o elemento atual na próxima posição
    mov [esi], bl                 ; coloca o próximo elemento na posição atual
    inc edx                       ; incrementa o contador de trocas

no_swap:
    inc esi                       ; incrementa o ponteiro do vetor
    loop loop_inner               ; repete até percorrer todo o vetor

    cmp edx, 0                    ; verifica se houve trocas nesta iteração
    jne loop_outer                ; se houve, repete o processo

    mov edx, 1                    ; indica que o vetor está ordenado

print_vetor:
    mov eax, 4                    ; syscall para escrever na saída padrão
    mov ebx, 1                    ; descritor de arquivo para stdout
    mov ecx, vetor                ; endereço inicial do vetor
    mov edx, tamanho              ; quantidade de elementos no vetor
    add edx, 1                    ; incrementa para imprimir o último elemento
    mov eax, 4                    ; syscall para escrever na saída padrão
    int 0x80                      ; interrupção para realizar a syscall

exit:
    mov eax, 1                    ; syscall para terminar o programa
    xor ebx, ebx                  ; código de retorno 0
    int 0x80                      ; interrupção para realizar a syscall
```

Neste código, primeiro definimos o vetor de teste com alguns números desordenados. Em seguida, definimos o tamanho do vetor usando a diretiva `equ`. 

Na seção `.text`, começamos com o rótulo `_start`, que é o ponto de entrada do programa. 

Em seguida, usamos o registrador `ecx` para guardar a quantidade de elementos no vetor e decrementamos esse valor para ajustar o índice final. 

Em um loop externo (`loop_outer`), percorremos o vetor repetidamente até que não haja mais trocas. 

Dentro desse loop, usamos o registrador `esi` para guardar o endereço inicial do vetor e o registrador `edx` para contar o número de trocas. 

Em um loop interno (`loop_inner`), comparamos cada elemento com o próximo elemento do vetor. Se o elemento atual for maior que o próximo elemento, realizamos uma troca e incrementamos o contador de trocas. 

Após percorrer todo o vetor, verificamos se houve trocas nesta iteração. Se não houver trocas, significa que o vetor está ordenado e pulamos para a etapa de impressão do vetor. 

Na etapa de impressão, usamos a syscall `write` para imprimir o vetor ordenado na saída padrão. 

Por fim, utilizamos a syscall `exit` para terminar o programa.