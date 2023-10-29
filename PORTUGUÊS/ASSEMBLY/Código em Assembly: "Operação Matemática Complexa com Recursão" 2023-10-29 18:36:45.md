Claro! Aqui está um código em Assembly que realiza uma operação matemática complexa, utilizando recursão e registradores de uso geral:

```assembly
section .data
    result db 0 ; variável para armazenar o resultado

section .text
    global _start
    
_start:
    ; chama a função complexa com o número 5
    mov eax, 5
    call funcao_complexa
    
    ; exibe o resultado
    mov eax, 4
    mov ebx, 1
    mov ecx, result
    mov edx, 1
    int 0x80
    
    ; saída do programa
    mov eax, 1
    xor ebx, ebx
    int 0x80
    
funcao_complexa:
    ; salva o valor do registrador eax na pilha
    push eax
    
    ; verifica se o número é igual a zero
    cmp eax, 0
    je fim_funcao
    
    ; realiza uma operação matemática complexa
    ; neste exemplo, vamos apenas adicionar 1 ao resultado
    add byte [result], 1
    
    ; chama a função complexa recursivamente
    sub eax, 1
    call funcao_complexa
    
fim_funcao:
    ; restaura o valor do registrador eax e retorna
    pop eax
    ret
```

Neste código em Assembly, temos uma função chamada `funcao_complexa` que recebe um número inteiro em `eax` e realiza uma operação matemática complexa recursivamente. Neste exemplo, a função simplesmente adiciona 1 ao resultado. 

O código principal começa chamando a função `funcao_complexa` com o número 5. Após a chamada da função, o resultado é exibido na saída padrão usando a chamada de sistema `int 0x80` com a syscall 4. Por fim, o programa é encerrado com a syscall 1.

Vale ressaltar que esse código é apenas um exemplo e pode ser adaptado de acordo com suas necessidades.