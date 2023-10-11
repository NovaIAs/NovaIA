Claro! Aqui está um exemplo de um código complexo em Assembly que utiliza a linguagem de montagem Intel x86 para realizar uma tarefa específica: calcular o fatorial de um número fornecido pelo usuário.

```assembly
section .data
    message db "Digite um número inteiro positivo: ", 0
    result db "O fatorial do número é: ", 0

section .bss
    number resb 2
    factorial resb 4

section .text
    global _start

_start:
    ; Imprime a mensagem solicitando o número
    mov eax, 4
    mov ebx, 1
    mov ecx, message
    mov edx, 29
    int 0x80

    ; Lê o número fornecido pelo usuário
    mov eax, 3
    mov ebx, 0
    mov ecx, number
    mov edx, 2
    int 0x80

    ; Converte o número lido para um valor inteiro
    mov eax, 0
    movzx ebx, byte [number]
    sub ebx, '0'
    mov ecx, 10
    mul ecx
    add eax, ebx

    ; Calcula o fatorial do número
    mov ebx, eax
    mov ecx, eax
    dec ecx

    cmp eax, 0
    jle print_result

calculate_factorial:
    mul ecx
    loop calculate_factorial

print_result:
    ; Converte o fatorial calculado para uma string
    mov ebx, 10
    xor ecx, ecx
    mov edi, factorial

convert_to_string:
    xor edx, edx
    div ebx
    add dl, '0'
    mov [edi], dl
    inc edi
    inc ecx
    test eax, eax
    jnz convert_to_string

    ; Inverte a string do fatorial
    mov esi, factorial
    dec edi

reverse_string:
    mov al, [esi]
    mov dl, [edi]
    mov [edi], al
    mov [esi], dl
    inc esi
    dec edi
    cmp esi, edi
    jl reverse_string

    ; Imprime o resultado
    mov eax, 4
    mov ebx, 1
    mov ecx, result
    mov edx, 22
    int 0x80

    mov eax, 4
    mov ebx, 1
    mov ecx, factorial
    mov edx, ecx
    int 0x80

exit:
    ; Encerra o programa
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

Este código em Assembly apresenta um exemplo complexo que realiza a tarefa de calcular o fatorial de um número inteiro positivo fornecido pelo usuário. Ele começa imprimindo uma mensagem solicitando ao usuário para digitar um número. Em seguida, lê o número fornecido pelo usuário e o converte em um valor inteiro.

Depois disso, o código calcula o fatorial do número utilizando um loop que multiplica o número atual pela iteração anterior até chegar a 1. Em seguida, converte o resultado do fatorial em uma string e inverte a ordem dos caracteres para imprimir corretamente o resultado.

Finalmente, o código imprime a mensagem "O fatorial do número é:" seguida pelo resultado calculado e, em seguida, encerra o programa.

Embora este código possa parecer complexo e grande, ele demonstra várias funcionalidades e técnicas comuns na programação em Assembly, como manipulação de strings, conversão de números e uso de loops.