Claro! Aqui está um código em Assembly que realiza a soma de dois números inteiros e exibe o resultado:

```assembly
section .data
    num1 db 10   ; primeiro número
    num2 db 5    ; segundo número
    result db 0  ; resultado da soma

section .text
    global _start

_start:
    ; carrega o primeiro número em AL
    mov al, [num1]

    ; carrega o segundo número em BL
    mov bl, [num2]

    ; soma os números e armazena o resultado em AL
    add al, bl

    ; converte o resultado para um caractere ASCII
    add al, '0'

    ; armazena o resultado na variável result
    mov [result], al

    ; exibe o resultado
    mov eax, 4  ; número da syscall write
    mov ebx, 1  ; número do descritor de arquivo (stdout)
    mov ecx, result ; endereço do resultado
    mov edx, 1  ; tamanho do resultado (1 byte)
    int 0x80    ; chama a interrupção do sistema

    ; termina a execução do programa
    mov eax, 1  ; número da syscall exit
    xor ebx, ebx ; código de saída (0)
    int 0x80    ; chama a interrupção do sistema
```

Este código em Assembly realiza a soma de dois números inteiros, `num1` e `num2`, e armazena o resultado na variável `result`. Em seguida, converte o resultado para um caractere ASCII e exibe-o na saída padrão.

O código começa definindo três seções: `.data`, `.text` e `_start`. Na seção `.data`, são definidas as variáveis `num1`, `num2` e `result`, que são inicializadas com os valores desejados. Na seção `.text`, é definida a função `_start`, que é o ponto de entrada do programa.

Dentro da função `_start`, os valores dos números são carregados nos registradores `AL` e `BL`. Em seguida, é utilizada a instrução `add` para somar os valores de `AL` e `BL`, armazenando o resultado em `AL`. Após a soma, o resultado é convertido para um caractere ASCII adicionando-se o valor do caractere `'0'` a `AL`.

O resultado convertido é então armazenado na variável `result` usando a instrução `mov`. Em seguida, o resultado é exibido na saída padrão usando a syscall `write`.

Por fim, o programa termina chamando a syscall `exit` com o código de saída igual a 0. Assim, a execução do programa é encerrada.