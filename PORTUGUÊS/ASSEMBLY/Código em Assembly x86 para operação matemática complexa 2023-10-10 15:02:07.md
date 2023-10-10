Certamente! Aqui está um código em Assembly x86 que realiza uma operação matemática complexa utilizando múltiplos registradores e manipulação de memória:

```assembly
section .data
    resultado db 0

section .text
    global _start

_start:
    ; Inicializar registradores
    mov eax, 5
    mov ebx, 3
    mov ecx, 2

    ; Realizar operações matemáticas
    add eax, ebx      ; eax = eax + ebx
    sub ecx, ebx      ; ecx = ecx - ebx
    imul eax, ecx     ; eax = eax * ecx
    idiv ebx          ; eax = eax / ebx

    ; Armazenar resultado na memória
    mov [resultado], eax

    ; Encerrar programa
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

Este código começa definindo uma seção de dados (`section .data`) onde é reservado um espaço de memória chamado `resultado`, que será usado para armazenar o resultado final da operação.

Em seguida, temos a seção de código (`section .text`) onde definimos o ponto de entrada do programa (`_start`).

Dentro do `_start`, os registradores `eax`, `ebx` e `ecx` são inicializados com valores específicos (respectivamente, 5, 3 e 2) para serem utilizados nas operações matemáticas.

A sequência de instruções utiliza diferentes instruções Assembly para realizar as operações matemáticas desejadas. Primeiro, a soma de `eax` e `ebx` é realizada com a instrução `add`. Em seguida, a subtração de `ebx` de `ecx` é feita com a instrução `sub`. Depois, a multiplicação de `eax` por `ecx` é executada usando a instrução `imul`. Por fim, a divisão de `eax` por `ebx` é realizada com a instrução `idiv`.

Após as operações matemáticas, o valor resultante é armazenado na memória, na posição definida por `resultado`, utilizando a instrução `mov [resultado], eax`.

Finalmente, o programa é encerrado com as instruções para sair do programa, onde `eax` é definido como 1 para indicar que o programa foi executado com sucesso, `xor ebx, ebx` é utilizado para limpar o registrador `ebx` e `int 0x80` é usado para chamar a interrupção de sistema que encerra o programa.

Espero que este código atenda à sua solicitação!