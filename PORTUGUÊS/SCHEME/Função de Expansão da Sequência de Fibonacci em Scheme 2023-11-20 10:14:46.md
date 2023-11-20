Claro! Aqui está um código em Scheme que implementa a função de expansão de uma sequência de Fibonacci. A função `fibonacci` recebe um argumento `n` e retorna a sequência de Fibonacci até o `n`-ésimo termo.

```scheme
(define (fibonacci n)
  (define (fib-iter a b count)
    (if (>= count n)
        '()
        (cons a (fib-iter b (+ a b) (+ count 1)))))
  (fib-iter 0 1 0))
```

O código acima define no escopo global a função `fibonacci`, que é recursiva através da função interna `fib-iter`. 

No `fib-iter`, temos três argumentos: `a`, `b` e `count`. `a` é o valor atual da sequência de Fibonacci, `b` é o próximo valor e `count` é um contador da iteração atual.

A função `fib-iter` verifica se o contador `count` é maior ou igual a `n`. Se for, isso significa que já atingimos o final da sequência e retorna uma lista vazia `()`.

Caso contrário, a função `fib-iter` continua aplicando a iteração recursiva. Ela usa a função `cons` para adicionar o valor `a` na frente do resultado da iteração recursiva, que é chamada com `b` como novo `a`, a soma de `a` e `b` como novo `b`, e o incremento de `count`.

Assim, a função `fibonacci` chama `fib-iter` com valores iniciais de `0` para `a`, `1` para `b` e `0` para `count`, retornando a sequência de Fibonacci até o número `n`.

Espero que este código atenda às suas necessidades!