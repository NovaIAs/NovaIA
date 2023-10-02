Claro! Aqui está um exemplo de código em Assembly x86 que realiza o cálculo do fatorial de um número:

```assembly
section .data
    msg db "Digite um número: ", 0
    result_msg db "O fatorial é: ", 0

section .bss
    num resb 2

section .text
    global _start

_start:
    ; Chama a função para exibir a mensagem
    mov eax, 4
    mov ebx, 1
    mov ecx, msg
    mov edx, 16
    int 0x80

    ; Lê o número digitado pelo usuário
    mov eax, 3
    mov ebx, 0
    mov ecx, num
    mov edx, 2
    int 0x80

    ; Converte o número para inteiro
    xor eax, eax
    xor ebx, ebx
    mov bl, [num]
    sub bl, '0'

    ; Calcula o fatorial
    mov eax, 1
    mov ecx, ebx
    dec ecx

loop_start:
    imul eax, ecx
    loop loop_start

    ; Converte o resultado para string
    xor ebx, ebx
    mov bl, al
    add bl, '0'

    ; Chama a função para exibir o resultado
    mov eax, 4
    mov ebx, 1
    mov ecx, result_msg
    mov edx, 13
    int 0x80

    ; Exibe o resultado
    mov eax, 4
    mov ebx, 1
    mov ecx, ebx
    mov edx, 1
    int 0x80

exit:
    ; Encerra o programa
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

Esse código solicita ao usuário um número e calcula o fatorial desse número. Em seguida, exibe o resultado na tela.