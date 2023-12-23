Certamente! Aqui está um código em Assembly que realiza a multiplicação de dois números inteiros de 32 bits, utilizando a arquitetura x86. O código está comentado em português para facilitar o entendimento:

```assembly
section .data
    resultado db "O resultado da multiplicação é: %d", 0
    ; String de formatação para imprimir o resultado
    num1 dd 10 ; Primeiro número a ser multiplicado (valor inicial: 10)
    num2 dd 5 ; Segundo número a ser multiplicado (valor inicial: 5)

section .text
    global _start

_start:
    ; Multiplicação: num1 * num2
    mov eax, [num1] ; Move o valor de num1 para o registrador eax
    mov ebx, [num2] ; Move o valor de num2 para o registrador ebx
    imul eax, ebx ; Multiplica eax por ebx e armazena o resultado em eax

    ; Imprime o resultado
    push eax ; Coloca o valor de eax na pilha
    push resultado ; Coloca o endereço da string de formatação na pilha
    call printf ; Chama a função printf para imprimir a string formatada
    add esp, 8 ; Ajusta o valor do ponteiro de pilha após a chamada da função

exit:
    mov eax, 1 ; Define o código de saída 1 (encerramento do programa)
    xor ebx, ebx ; Limpa o registrador ebx (usado para sinalizar um encerramento normal)
    int 0x80 ; Chama a interrupção do sistema operacional para encerrar o programa
```

Este código em Assembly utiliza a função `printf` para imprimir o resultado da multiplicação dos números `num1` e `num2`, que são definidos como 10 e 5, respectivamente. A multiplicação é feita utilizando a instrução `imul` e o resultado é armazenado no registrador `eax`. Em seguida, o valor de `eax` é colocado na pilha e a função `printf` é chamada para imprimir a string formatada contendo o resultado da multiplicação. Por fim, o programa é encerrado com a interrupção `int 0x80`.