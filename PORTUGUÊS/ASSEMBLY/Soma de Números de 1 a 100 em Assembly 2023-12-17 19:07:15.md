Certamente! Aqui está um código em Assembly, com comentários em português, que realiza a soma de todos os números de 1 a 100 e imprime o resultado:

```assembly
section .data
    soma db 0 ; variável para armazenar a soma
    i db 1   ; variável de controle do loop
    msg db 'A soma de todos os numeros de 1 a 100 eh: %d', 0 ; mensagem a ser exibida

section .text
    global _start

_start:
    mov ecx, 100 ; define o número de iterações do loop
    mov eax, 0   ; zera o registrador eax

loop_start:
    add eax, ecx ; adiciona o valor de ecx (100) ao eax
    dec ecx      ; decrementa ecx

    cmp ecx, 0   ; verifica se ecx é igual a zero
    jne loop_start ; se não for zero, volta ao início do loop

    mov [soma], eax ; armazena o resultado da soma na variável 'soma'

    ; parte do código para exibir o resultado
    push eax      ; coloca o valor de eax na pilha
    push msg      ; coloca o endereço da mensagem na pilha
    call printf   ; chama a função printf
    add esp, 8    ; corrige o ponteiro da pilha

    ; finaliza o programa
    mov eax, 1    ; código de saída
    xor ebx, ebx  ; status de saída
    int 0x80      ; interrupção do sistema

section .data
    extern printf ; declaração externa da função printf
```

Neste código, utilizamos o NASM Assembly, que é muito comum na plataforma x86. O código começa definindo duas variáveis: `soma`, para armazenar o resultado da soma, e `i`, como variável de controle do loop. Em seguida, temos a declaração da mensagem que será exibida.

Na seção `.text`, o ponto de entrada do programa é definido como `_start`. Primeiro, movemos o valor 100 para o registrador `ecx`, que será utilizado como contador do loop. Em seguida, zeramos o registrador `eax` para iniciar a soma.

Dentro do loop, adicionamos o valor de `ecx` ao registrador `eax` e decrementamos `ecx` em cada iteração. Em seguida, comparamos `ecx` com zero para verificar se o loop deve continuar. Caso `ecx` seja diferente de zero, voltamos ao início do loop.

Após o loop, armazenamos o valor final da soma na variável `soma`. Em seguida, utilizamos a função `printf` para exibir a mensagem formatada com o valor da soma. Por fim, finalizamos o programa com o código de saída 1.

É importante ressaltar que este é apenas um exemplo de código Assembly, e sua complexidade pode variar de acordo com a finalidade do programa.