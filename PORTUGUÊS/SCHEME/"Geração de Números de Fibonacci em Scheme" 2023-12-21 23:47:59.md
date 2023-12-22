Claro, vou gerar um código complexo em Scheme para você. Lembre-se de que Scheme é uma linguagem de programação funcional e minimalista, portanto, o código pode parecer um pouco diferente do que você está acostumado. Aqui está um exemplo de código em Scheme que gera os números de Fibonacci:

```scheme
;; Definindo a função fibonacci
(define (fibonacci n)
  (letrec ((fib (lambda (n a b)
                  (if (= n 0)
                      a
                      (fib (- n 1) b (+ a b))))))
    (fib n 0 1)))

;; Função auxiliar para imprimir os números de Fibonacci
(define (imprimir-fibonacci n)
  (do ((i 0 (+ i 1)))
      ((>= i n))
    (display (fibonacci i))
    (newline)))

;; Chamando a função para imprimir os primeiros 10 números de Fibonacci
(imprimir-fibonacci 10)
```

Explicação do código:

1. A função `fibonacci` é definida usando a forma especial `letrec`, que permite a definição de funções recursivas. Essa função recebe um número `n` e retorna o `n`-ésimo número de Fibonacci.
2. A função `imprimir-fibonacci` é definida para imprimir os primeiros `n` números de Fibonacci. Ela usa um loop `do` para iterar de 0 até `n-1`, chamando a função `fibonacci` e imprimindo o resultado.
3. Por fim, chamamos a função `imprimir-fibonacci` com o argumento 10 para imprimir os primeiros 10 números de Fibonacci.

Espero que esse exemplo atenda às suas expectativas. Lembre-se de que Scheme é uma linguagem poderosa, mas também pode ser um pouco complexa de entender no início. Se tiver alguma dúvida, fique à vontade para perguntar!