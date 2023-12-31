Claro! Aqui está um código em Assembly que realiza uma operação complexa de manipulação de dados em português:

```
; Código em Assembly para realizar a operação complexa de manipulação de dados em português

section .data
    mensagem db 'Bem-vindo ao programa de manipulação de dados!', 0
    valor1 dd 0
    valor2 dd 0
    resultado dd 0

section .text
    global _start

_start:
    ; Exibir mensagem de boas-vindas
    mov eax, 4
    mov ebx, 1
    mov ecx, mensagem
    mov edx, 41
    int 0x80

    ; Ler valor1 do usuário
    mov eax, 3
    mov ebx, 0
    mov ecx, valor1
    mov edx, 4
    int 0x80

    ; Ler valor2 do usuário
    mov eax, 3
    mov ebx, 0
    mov ecx, valor2
    mov edx, 4
    int 0x80

    ; Realizar a manipulação de dados
    mov eax, [valor1]
    mov ebx, [valor2]
    add eax, ebx
    mov [resultado], eax

    ; Exibir o resultado
    mov eax, 4
    mov ebx, 1
    mov ecx, resultado
    mov edx, 4
    int 0x80

    ; Finalizar o programa
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

Este código em Assembly exibe uma mensagem de boas-vindas ao usuário e, em seguida, lê dois valores fornecidos pelo usuário. Em seguida, ele realiza uma manipulação de dados, que neste caso é uma simples adição dos dois valores fornecidos. O resultado da operação é exibido na saída padrão. O programa então é finalizado.