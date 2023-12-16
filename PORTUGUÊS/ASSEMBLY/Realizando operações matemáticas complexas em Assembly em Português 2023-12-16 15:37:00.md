Claro! Aqui está um exemplo de código em Assembly que realiza uma operação matemática complexa em português:

```
; Exemplo de código em Assembly que realiza uma operação matemática complexa
section .data
    resultado db 0

section .text
    global _start

_start:
    ; Configurações iniciais
    mov eax, 20
    mov ebx, 5
    mov ecx, 10

    ; Faz a primeira operação matemática (eax + ebx * ecx)
    add eax, ebx
    imul eax, ecx

    ; Faz a segunda operação matemática (eax - ebx)
    sub eax, ebx

    ; Verifica se o resultado é positivo ou negativo
    cmp eax, 0
    jge positivo
    jl negativo

positivo:
    ; Converte o resultado em número hexadecimal
    mov edx, eax
    mov eax, 16
    xor ebx, ebx
    div ebx
    mov edi, edx
    mov eax, edi
    add al, 30h
    cmp al, 3ah
    jle hex_digit
    add al, 7h

hex_digit:
    mov [resultado], al

    ; Termina o programa
    mov eax, 1
    xor ebx, ebx
    int 0x80

negativo:
    ; Converte o resultado em número binário
    mov ecx, 32
    mov edi, 1
convert_loop:
    shl eax, 1
    adc edi, 0
    loop convert_loop

    ; Armazena o resultado em uma string
    mov esi, resultado
    mov edi, 32
store_loop:
    mov dl, '0'
    test edi, eax
    jz zero
    mov dl, '1'

zero:
    mov [esi], dl
    inc esi
    dec edi
    test edi, edi
    jnz store_loop

    ; Termina o programa
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

Este código em Assembly realiza duas operações matemáticas complexas: `(eax + ebx * ecx) - ebx`. Em seguida, verifica se o resultado é positivo ou negativo. Se for positivo, converte o resultado para um número hexadecimal e armazena o dígito no endereço de memória "resultado". Se for negativo, converte o resultado para um número binário e armazena a representação em uma string. Finalmente, o programa é terminado.

Observe que este código é apenas um exemplo e pode ser adaptado para atender a requisitos específicos.