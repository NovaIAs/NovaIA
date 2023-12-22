Claro! Aqui está um código complexo em Scheme que utiliza funções recursivas para calcular o fatorial de um número:

```
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))
  )
)

(define (somatorio n)
  (if (= n 0)
      0
      (+ n (somatorio (- n 1)))
  )
)

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))
  )
)

(define (potencia base expoente)
  (if (= expoente 0)
      1
      (* base (potencia base (- expoente 1)))
  )
)

(display "Digite um número para calcular seu fatorial: ")
(define num (read))

(display "O fatorial de ")
(display num)
(display " é ")
(display (fatorial num))
(newline)

(display "Digite um número para calcular o somatório: ")
(define num (read))

(display "O somatório de 1 até ")
(display num)
(display " é ")
(display (somatorio num))
(newline)

(display "Digite um número para calcular o n-ésimo termo da sequência de Fibonacci: ")
(define num (read))

(display "O n-ésimo termo da sequência de Fibonacci com n = ")
(display num)
(display " é ")
(display (fibonacci num))
(newline)

(display "Digite a base: ")
(define base (read))

(display "Digite o expoente: ")
(define expoente (read))

(display "A potência de ")
(display base)
(display " elevado a ")
(display expoente)
(display " é ")
(display (potencia base expoente))
(newline)
```

Explicando o código:

1. Primeiro, definimos a função `fatorial`, que calcula o fatorial de um número `n` utilizando recursão. Se `n` for menor ou igual a 1, retorna 1; caso contrário, retorna o produto entre `n` e o fatorial de `n-1`.

2. Em seguida, definimos a função `somatorio`, que calcula o somatório de 1 até `n` utilizando recursão. Se `n` for igual a 0, retorna 0; caso contrário, retorna a soma entre `n` e o somatório de `n-1`.

3. A função `fibonacci` calcula o n-ésimo termo da sequência de Fibonacci utilizando recursão. Se `n` for igual a 0 ou 1, retorna `n`; caso contrário, retorna a soma entre o (n-1)-ésimo termo e o (n-2)-ésimo termo.

4. A função `potencia` calcula a potência de um número `base` elevado a um `expoente`. Se `expoente` for igual a 0, retorna 1; caso contrário, retorna o produto entre `base` e a potência de `base` elevado a `(expoente - 1)`.

5. O código então solicita ao usuário que digite um número para calcular o fatorial, em seguida chama a função `fatorial` com o número fornecido e imprime o resultado.

6. Da mesma forma, o código solicita ao usuário que digite um número para calcular o somatório, chama a função `somatorio` com o número fornecido e imprime o resultado.

7. Em seguida, o código solicita ao usuário que digite um número para calcular o n-ésimo termo da sequência de Fibonacci, chama a função `fibonacci` com o número fornecido e imprime o resultado.

8. Por fim, o código solicita ao usuário que digite a base e o expoente para calcular a potência, chama a função `potencia` com os valores fornecidos e imprime o resultado.

Esse código utiliza funções recursivas para calcular diferentes operações matemáticas, demonstrando o poder da recursão na linguagem Scheme.