Claro! Aqui está um código complexo e diferenciado em Assembly, escrito em português:

```
section .data
    mensagem db 'Bem-vindo ao mundo da programação Assembly!', 0

section .text
    global _start

_start:
    ; Configura o registrador de saída para imprimir no console
    mov eax, 4
    mov ebx, 1
    mov ecx, mensagem
    mov edx, 36 ; tamanho da mensagem
    int 0x80

    ; Pula para a próxima instrução
    jmp imprimir_asteriscos

imprimir_asteriscos:
    ; Configura o registrador de saída para imprimir no console
    mov eax, 4
    mov ebx, 1
    
    ; Loop para imprimir asteriscos
    mov ecx, 10 ; quantidade de asteriscos
    mov edx, 1 ; tamanho de cada asterisco
    mov al, '*'
    imprimir:
        int 0x80
        dec ecx
        jnz imprimir

    ; Pula para a próxima instrução
    jmp calcular_soma

calcular_soma:
    ; Configura os registradores para realizar a soma
    mov eax, 5
    mov ebx, 10
    add eax, ebx

    ; Pula para a próxima instrução
    jmp verificar_paridade

verificar_paridade:
    ; Verifica se o resultado da soma é par ou ímpar
    mov ebx, 2
    xor edx, edx
    div ebx

    ; Verifica o resultado da divisão
    cmp edx, 0
    je resultado_par
    jne resultado_impar

resultado_par:
    ; Configura o registrador de saída para imprimir no console
    mov eax, 4
    mov ebx, 1
    mov ecx, par
    mov edx, 7 ; tamanho da mensagem
    int 0x80

    ; Pula para a próxima instrução
    jmp finalizar

resultado_impar:
    ; Configura o registrador de saída para imprimir no console
    mov eax, 4
    mov ebx, 1
    mov ecx, impar
    mov edx, 9 ; tamanho da mensagem
    int 0x80

finalizar:
    ; Termina o programa
    mov eax, 1
    xor ebx, ebx
    int 0x80

section .data
    par db 'O resultado é par!', 0
    impar db 'O resultado é ímpar!', 0
```

Este código em Assembly começa imprimindo uma mensagem de boas-vindas no console. Em seguida, ele imprime 10 asteriscos. Depois, o código realiza uma soma entre 5 e 10 e verifica se o resultado é par ou ímpar. Por fim, ele imprime a mensagem correspondente ao resultado da soma (par ou ímpar) e encerra o programa.

Espero que este código atenda às suas expectativas! Se tiver alguma outra solicitação, por favor, deixe-me saber.