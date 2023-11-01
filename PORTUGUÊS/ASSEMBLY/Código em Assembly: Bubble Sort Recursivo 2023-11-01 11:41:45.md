Claro! Aqui está um código em Assembly que implementa um algoritmo de ordenação chamado Bubble Sort de forma recursiva. O Bubble Sort é um algoritmo simples, porém ineficiente para grandes quantidades de dados.

```
section .data
    array db 8, 4, 2, 9, 3, 6, 1, 5, 7 ; Array a ser ordenado
    length equ $ - array ; Tamanho do array

section .text
    global _start

_start:
    mov eax, length
    dec eax ; Índice final do array

bubbleSort:
    cmp eax, 0
    jle endSort ; Se eax <= 0, termina o algoritmo

    mov ebx, eax ; Índice atual

innerLoop:
    mov edx, ebx
    dec edx ; Índice anterior

    mov al, [array + ebx]
    mov dl, [array + edx]

    cmp al, dl
    jge outerLoop ; Se al >= dl, vá para o próximo elemento

    mov [array + ebx], dl ; Troca os elementos
    mov [array + edx], al

outerLoop:
    dec ebx ; Próximo índice
    cmp ebx, 0
    jge innerLoop ; Se ebx >= 0, volte para o innerLoop

    dec eax ; Próximo elemento do array
    jmp bubbleSort ; Volte para o bubbleSort

endSort:
    ; Imprimir o array ordenado
    mov eax, 4
    mov ebx, 1
    mov ecx, array
    mov edx, length
    sub edx, 1 ; Remover a última vírgula

    add edx, '0' ; Converter para caractere
    push edx ; Empilhar o tamanho do array
    mov edx, esp

    int 0x80

exit:
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

Este código em Assembly implementa o algoritmo de ordenação Bubble Sort de forma recursiva. Primeiro, declaramos a seção `.data` para definir o array a ser ordenado. Em seguida, declaramos a seção `.text` com a função _start, que é o ponto de entrada do programa.

Dentro da função _start, carregamos o tamanho do array em EAX, decrementamos EAX para obter o índice final do array e chamamos a função bubbleSort.

A função bubbleSort começa comparando o índice final do array (em EAX) com zero. Se for menor ou igual a zero, o algoritmo termina. Caso contrário, o índice atual é armazenado em EBX.

Em seguida, temos um loop interno (innerLoop) que compara os elementos adjacentes do array. Se o elemento atual (em AL) for menor que o elemento anterior (em DL), eles são trocados.

Depois do loop interno, decrementamos o índice atual (EBX) e verificamos se ele é maior ou igual a zero. Se for, voltamos para o innerLoop. Caso contrário, decrementamos o índice final (EAX) e chamamos a função bubbleSort recursivamente.

Quando o algoritmo termina de ordenar o array, o código imprime o array ordenado usando a syscall `write`. Finalmente, o código chama a syscall `exit` para encerrar o programa.

Este código é apenas um exemplo de implementação do Bubble Sort em Assembly e pode ser otimizado e melhorado de várias maneiras.