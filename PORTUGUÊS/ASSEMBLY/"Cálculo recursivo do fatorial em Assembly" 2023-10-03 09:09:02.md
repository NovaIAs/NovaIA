Claro! Aqui está um exemplo de código em Assembly que calcula o fatorial de um número usando uma abordagem recursiva:

```assembly
section .data
    numero db 5            ; número para calcular o fatorial
    resultado db 0         ; variável para armazenar o resultado

section .text
    global _start

_start:
    mov al, [numero]       ; carrega o número para o registrador al
    call fatorial          ; chama a função fatorial

    mov [resultado], al    ; armazena o resultado na variável resultado

    ; Imprime o resultado
    mov edx, 1             ; comprimento da string
    mov ecx, resultado     ; endereço da string
    mov ebx, 1             ; file descriptor para stdout
    mov eax, 4             ; número do sistema para sys_write
    int 0x80               ; chama o sistema operacional

exit:
    mov eax, 1             ; número do sistema para sys_exit
    xor ebx, ebx           ; código de saída 0
    int 0x80               ; chama o sistema operacional

; Função recursiva para calcular o fatorial
fatorial:
    push ebp               ; salva o valor atual do ebp
    mov ebp, esp           ; define o ebp como o esp atual
    sub esp, 4             ; reserva espaço para a variável local n

    mov eax, [ebp+8]       ; carrega o valor do argumento passado para a função
    cmp eax, 1             ; verifica se o valor é igual a 1
    jle fim                ; se sim, pula para o fim da função

    dec eax                ; decrementa o valor de eax
    push eax               ; empurra o valor decrementado para a pilha
    call fatorial          ; chama a função fatorial recursivamente
    mov ebx, eax           ; armazena o resultado na variável ebx

    mov eax, [ebp+8]       ; carrega novamente o valor do argumento passado para a função
    imul ebx               ; multiplica o valor pelo resultado do fatorial

fim:
    add esp, 4             ; libera o espaço reservado para a variável local n
    pop ebp                ; restaura o valor anterior do ebp
    ret                    ; retorna da função
```

Este código em Assembly calcula o fatorial de um número usando uma função recursiva. A função `fatorial` é chamada passando o número a ser calculado como argumento. A função verifica se o número é igual a 1 e, caso contrário, chama a si mesma recursivamente com o valor decrementado. O resultado do fatorial é armazenado em `ebx` e, no final, é impresso na tela.

É importante ressaltar que esse código é apenas um exemplo e pode não funcionar em todos os sistemas operacionais ou ambientes de desenvolvimento específicos. Por isso, é necessário adaptá-lo às necessidades e ao contexto em que será utilizado.