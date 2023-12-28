Claro! Aqui está um código complexo em Scheme:

```scheme
(define (calcular-fatorial n)
  (if (= n 0)
      1
      (* n (calcular-fatorial (- n 1)))))

(define (calcular-soma-fatorial n)
  (define (auxiliar x soma)
    (if (= x 0)
        soma
        (auxiliar (- x 1) (+ soma (calcular-fatorial x)))))
  (auxiliar n 0))

(define (imprimir-soma-fatorial n)
  (display "A soma dos fatoriais de 1 até ")
  (display n)
  (display " é: ")
  (display (calcular-soma-fatorial n))
  (newline))

(define (main)
  (display "Digite um número inteiro positivo: ")
  (define numero (read))
  (imprimir-soma-fatorial numero))

(main)
```

Neste código, definimos três funções:

1. A função `calcular-fatorial` calcula o fatorial de um número `n` usando recursão. Se `n` for igual a 0, retorna 1. Caso contrário, retorna o resultado da multiplicação de `n` pelo fatorial do número `n-1`.

2. A função `calcular-soma-fatorial` calcula a soma dos fatoriais de 1 até `n`. Ela utiliza uma função auxiliar `auxiliar`, que recebe dois parâmetros: `x` (o número atual) e `soma` (o acumulador da soma dos fatoriais). A função `auxiliar` utiliza recursão para iterar de `n` até 1, somando o fatorial de cada número ao valor acumulado `soma`.

3. A função `imprimir-soma-fatorial` imprime na tela a mensagem "A soma dos fatoriais de 1 até `n` é: `soma`", onde `n` é o número informado e `soma` é o resultado da função `calcular-soma-fatorial`.

Por fim, a função `main` é responsável por ler um número inteiro positivo digitado pelo usuário, utilizando a função `read`, e chama a função `imprimir-soma-fatorial` passando esse número como argumento.

Este código calcula a soma dos fatoriais de 1 até um número inteiro positivo informado pelo usuário e exibe o resultado na tela. Cada fatorial é calculado usando a função `calcular-fatorial`, e a soma dos fatoriais é calculada usando a função `calcular-soma-fatorial`.