Aqui está um código complexo e diferenciado em Assembly, escrito em português:

```
; Este programa em Assembly recebe um número inteiro do usuário e calcula o fatorial desse número

section .data
    msg1 db "Digite um número inteiro positivo: ", 0
    msg2 db "O fatorial de ", 0
    msg3 db " é: ", 0
    msg4 db "Inválido! Digite um número inteiro positivo.", 0

section .bss
    num resb 2

section .text
    global _start

_start:
    ; exibe a mensagem solicitando um número ao usuário
    mov eax, 4
    mov ebx, 1
    mov ecx, msg1
    mov edx, 30
    int 0x80

    ; lê o número digitado pelo usuário
    mov eax, 3
    mov ebx, 0
    mov ecx, num
    mov edx, 2
    int 0x80

    ; converte o número digitado em ASCII para um número inteiro
    mov eax, 0
    mov al, byte [num]
    sub eax, 48
    mov ebx, eax

    ; verifica se o número é válido (positivo)
    cmp ebx, 0
    jl invalido

    ; calcula o fatorial do número
    mov ecx, ebx
    mov eax, 1

loop:
    mul ecx
    loop loop

    ; exibe o resultado do fatorial
    mov edx, 0
    mov ecx, msg2
    mov ebx, 1
    mov eax, 4
    int 0x80

    mov eax, 4
    mov ebx, 1
    mov ecx, num
    mov edx, 2
    int 0x80

    mov edx, 0
    mov ecx, msg3
    mov ebx, 1
    mov eax, 4
    int 0x80

    mov eax, 4
    mov ebx, 1
    mov ecx, num
    mov edx, 2
    int 0x80

    ; finaliza o programa
    jmp fim

invalido:
    ; exibe mensagem de número inválido
    mov eax, 4
    mov ebx, 1
    mov ecx, msg4
    mov edx, 40
    int 0x80

fim:
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

Este código em Assembly solicita ao usuário um número inteiro positivo e calcula seu fatorial. Primeiro, ele exibe a mensagem "Digite um número inteiro positivo:" para o usuário. Em seguida, lê o número digitado pelo usuário e converte-o de ASCII para um número inteiro.

Após verificar se o número é válido (positivo), o programa calcula o fatorial do número usando um loop e exibe o resultado na tela.

Caso o número digitado seja inválido (negativo ou zero), o programa exibe a mensagem "Inválido! Digite um número inteiro positivo."

Por fim, o programa termina sua execução.