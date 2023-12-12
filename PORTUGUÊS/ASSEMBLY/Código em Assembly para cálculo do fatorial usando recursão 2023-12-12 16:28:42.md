Claro! Aqui está um código em Assembly que demonstra o cálculo do fatorial de um número usando recursão. O código está comentado para ajudar na compreensão:

```assembly
section .data
    msg db "Digite um número: ", 0

section .bss
    num resb 2 ; espaço para armazenar o número digitado
    result resb 4 ; espaço para armazenar o resultado

section .text
    global _start

_start:
    ; exibe a mensagem para digitar um número
    mov eax, 4
    mov ebx, 1
    mov ecx, msg
    mov edx, 18
    int 0x80

    ; lê o número digitado
    mov eax, 3
    mov ebx, 0
    mov ecx, num
    mov edx, 2
    int 0x80

    ; converte o número lido para inteiro
    xor eax, eax
    mov al, [num]
    sub al, 0x30 ; subtrai o valor ASCII '0' para obter o valor numérico
    movzx eax, al

    ; chama a função fatorial
    push eax ; empilha o número para ser usado pela função
    call fatorial
    add esp, 4 ; remove o número empilhado

    ; converte o resultado para string
    push eax ; empilha o resultado para ser usado pela função itoa
    push result ; empilha o endereço do buffer de destino
    call itoa
    add esp, 8 ; remove os valores empilhados

    ; exibe o resultado
    mov eax, 4
    mov ebx, 1
    mov ecx, result
    mov edx, 4
    int 0x80

    ; finaliza o programa
    mov eax, 1
    xor ebx, ebx
    int 0x80

; função fatorial
fatorial:
    push ebp ; salva o valor atual do ponteiro de base
    mov ebp, esp ; define o ponteiro de base para o início da pilha

    mov eax, [ebp+8] ; obtém o número passado como argumento
    cmp eax, 1 ; compara com 1
    jle .fim ; se for menor ou igual a 1, pula para o final

    dec eax ; decrementa o número
    push eax ; empilha o número decrementado
    call fatorial ; chama recursivamente a função fatorial para o número decrementado
    mov ebx, eax ; o resultado da chamada recursiva é armazenado em ebx

    mov eax, [ebp+8] ; obtém novamente o número passado como argumento
    imul eax, ebx ; multiplica o número pelo resultado da chamada recursiva

.fim:
    mov esp, ebp ; restaura o ponteiro de pilha
    pop ebp ; desempilha o valor do ponteiro de base
    ret ; retorna da função

; função itoa (integer to ASCII)
itoa:
    push ebp ; salva o valor atual do ponteiro de base
    mov ebp, esp ; define o ponteiro de base para o início da pilha

    mov eax, [ebp+8] ; obtém o número passado como argumento
    mov ebx, 10 ; define a base decimal

    xor ecx, ecx ; contador de dígitos
    mov edi, 10000 ; divisor inicial (maior potência de 10)

    cmp eax, 0 ; verifica se o número é zero
    jne .calculo ; se não for zero, continua

    mov byte [edi-1], '0' ; se for zero, armazena o caractere '0' no buffer
    inc ecx ; incrementa o contador de dígitos

    jmp .fim

.calculo:
    cmp eax, 0 ; verifica se o número é zero
    jle .fim ; se for menor ou igual a zero, pula para o final

    xor edx, edx ; limpa edx
    div ebx ; divide eax por ebx, o resto fica em edx

    add dl, 0x30 ; converte o valor numérico em ASCII
    mov byte [edi-1], dl ; armazena o caractere no buffer

    dec edi ; decrementa o divisor
    inc ecx ; incrementa o contador de dígitos

    jmp .calculo ; repete o cálculo

.fim:
    mov eax, ecx ; retorna o número de dígitos
    mov esp, ebp ; restaura o ponteiro de pilha
    pop ebp ; desempilha o valor do ponteiro de base
    ret ; retorna da função
```

Esse código em Assembly implementa um programa em linguagem de máquina que solicita ao usuário um número inteiro e, em seguida, calcula o fatorial desse número usando recursão. O resultado é convertido em uma string e exibido na saída padrão.

O código utiliza as chamadas de sistema do Linux (int 0x80) para realizar operações de entrada/saída e interagir com o sistema operacional.

Espero que esse código atenda às suas expectativas! Se tiver mais alguma solicitação ou dúvida, estou à disposição.