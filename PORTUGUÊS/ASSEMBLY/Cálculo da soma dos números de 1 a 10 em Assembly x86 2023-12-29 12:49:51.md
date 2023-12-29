Certamente! Aqui está um exemplo de um código em Assembly x86 que calcula a soma dos números de 1 a 10 e exibe o resultado em português:

```assembly
section .data
    resultado db 'A soma dos numeros de 1 a 10 eh: ', 0
    numero db 0
    soma db 0

section .text
    global _start

_start:
    ; Inicializa o registrador ECX com 10 (quantidade de números a serem somados)
    mov ecx, 10

    ; Inicializa o registrador EBX com 1 (primeiro número a ser somado)
    mov ebx, 1

    ; Inicializa o registrador AL (Acumulador) com 0
    xor al, al

soma_loop:
    ; Adiciona o número atual ao acumulador
    add al, bl

    ; Incrementa o número atual
    inc bl

    ; Decrementa o contador
    dec ecx

    ; Verifica se ainda há números para serem somados
    jnz soma_loop

    ; Armazena o resultado da soma na variável 'soma'
    mov [soma], al

    ; Exibe a mensagem "A soma dos números de 1 a 10 é: "
    mov eax, 4
    mov ebx, 1
    mov edx, 32
    mov ecx, resultado
    int 0x80

    ; Exibe o resultado da soma
    mov eax, 4
    mov ebx, 1
    mov edx, 1
    mov ecx, soma
    int 0x80

exit:
    ; Encerra o programa
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

Explicação:

1. Primeiramente, definimos duas seções: `section .data` e `section .text`.
2. Na seção `.data`, declaramos três variáveis. `resultado` é uma string que armazena a mensagem a ser exibida, `numero` é uma variável temporária para armazenar o número atual durante a soma, e `soma` é onde o resultado final será armazenado.
3. Na seção `.text`, declaramos o ponto de entrada `_start`.
4. Dentro do `_start`, inicializamos o registrador `ECX` com o valor 10 (a quantidade de números a serem somados) e `EBX` com o valor 1 (o primeiro número a ser somado).
5. Em seguida, inicializamos o acumulador `AL` com 0 usando a instrução `xor al, al`.
6. Começamos um loop (`soma_loop`) para somar os números.
7. Dentro do loop, adicionamos o número atual ao acumulador usando a instrução `add al, bl`.
8. Incrementamos o número atual usando a instrução `inc bl`.
9. Decrementamos o contador usando a instrução `dec ecx`.
10. Verificamos se ainda há números para serem somados usando a instrução `jnz soma_loop`.
11. Armazenamos o resultado da soma na variável `soma` usando a instrução `mov [soma], al`.
12. Exibimos a mensagem "A soma dos números de 1 a 10 é: " usando as instruções `mov eax, 4`, `mov ebx, 1`, `mov edx, 32`, `mov ecx, resultado` e `int 0x80`.
13. Exibimos o resultado da soma usando as instruções `mov eax, 4`, `mov ebx, 1`, `mov edx, 1`, `mov ecx, soma` e `int 0x80`.
14. Por fim, encerramos o programa usando as instruções `mov eax, 1`, `xor ebx, ebx` e `int 0x80`.