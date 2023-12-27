Claro, vou criar um código complexo em Scheme para você. 

```scheme
;; Função recursiva para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função auxiliar para calcular o somatório dos fatoriais de um intervalo de números
(define (somatorio-fatoriais inicio fim)
  (if (> inicio fim)
      0
      (+ (fatorial inicio) (somatorio-fatoriais (+ inicio 1) fim))))

;; Função principal para encontrar a soma dos fatoriais dos números de 1 a n
(define (soma-fatoriais n)
  (somatorio-fatoriais 1 n))

;; Testando a função soma-fatoriais com o valor 5
(display (soma-fatoriais 5))
```

Neste código, temos três funções em Scheme:

1. A função `fatorial` é uma função recursiva que calcula o fatorial de um número. Ela verifica se o número é menor ou igual a 1 e, se for, retorna 1. Caso contrário, ela multiplica o número pelo fatorial do número anterior (calculado recursivamente).
2. A função `somatorio-fatoriais` é uma função auxiliar que recebe um intervalo de números `inicio` e `fim` e retorna o somatório dos fatoriais de todos os números nesse intervalo. Ela verifica se o `inicio` é maior que o `fim` e, se for, retorna 0. Caso contrário, ela soma o fatorial do `inicio` com o resultado da chamada recursiva passando `inicio+1` como novo `inicio`.
3. A função `soma-fatoriais` é a função principal que recebe um número `n` e retorna a soma dos fatoriais dos números de 1 a `n`. Ela chama a função `somatorio-fatoriais` passando 1 como `inicio` e `n` como `fim`.

No exemplo final, a função `soma-fatoriais` é testada com o valor 5 e o resultado é exibido na tela usando a função `display`.

Espero que esse código complexo em Scheme atenda às suas expectativas!