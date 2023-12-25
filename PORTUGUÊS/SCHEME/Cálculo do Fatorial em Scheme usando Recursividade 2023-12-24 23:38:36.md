Claro! Aqui está um código em Scheme que utiliza recursividade para calcular o fatorial de um número:

```scheme
(define (fatorial n)
  (if (or (not (integer? n)) (< n 0))
      (error "O fatorial só pode ser calculado para números inteiros não negativos!")
      (if (<= n 1)
          1
          (* n (fatorial (- n 1))))))

(display "Digite um número inteiro não negativo: ")
(define numero (read))
(display "O fatorial de ")
(display numero)
(display " é ")
(display (fatorial numero))
(newline)
```

Explicação:
1. Definimos a função `fatorial`, que recebe um número `n` como parâmetro.
2. Verificamos se `n` é um número inteiro não negativo. Caso contrário, lançamos um erro.
3. Se `n` for menor ou igual a 1, retornamos 1, pois o fatorial de 0 e 1 é igual a 1.
4. Caso contrário, multiplicamos `n` pelo fatorial do número anterior (`n - 1`) chamando recursivamente a função `fatorial`.
5. No final, pedimos ao usuário para digitar um número, armazenamos esse número na variável `numero` e exibimos o fatorial desse número.

Espero que este código atenda às suas necessidades!