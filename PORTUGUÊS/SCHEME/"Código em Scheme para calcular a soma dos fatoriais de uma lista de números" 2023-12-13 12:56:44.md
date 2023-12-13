Claro! Aqui está um código complexo em Scheme:

```scheme
;; Definição de uma função auxiliar para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Definição de uma função principal que utiliza a função auxiliar para calcular a soma dos fatoriais de uma lista de números
(define (soma-fatoriais lista)
  (define (iter soma lista)
    (if (null? lista)
        soma
        (iter (+ soma (fatorial (car lista))) (cdr lista))))
  (iter 0 lista))

;; Teste da função soma-fatoriais
(displayln "Digite uma lista de números separados por espaço:")
(define numeros (map string->number (string-split (read-line) " ")))
(displayln (format "A soma dos fatoriais é: ~a" (soma-fatoriais numeros)))
```

Explicação do código:

1. Começamos definindo a função `fatorial`, que recebe um número `n` como parâmetro. Esta função utiliza um condicional `if` para verificar se `n` é menor ou igual a 1. Se for, retorna 1 (caso base). Caso contrário, retorna o produto de `n` pelo fatorial do número `n-1`.
2. Em seguida, definimos a função `soma-fatoriais`, que recebe uma lista de números como parâmetro. Esta função utiliza uma função auxiliar `iter` para percorrer a lista e realizar a soma dos fatoriais dos números. Inicialmente, a soma é iniciada em 0.
3. A função `iter` possui dois parâmetros: `soma` e `lista`. Ela utiliza um condicional `if` para verificar se a lista é vazia. Se for, retorna a soma atual. Caso contrário, chama recursivamente a função `iter`, somando à soma atual o fatorial do primeiro número da lista e passando o restante da lista como parâmetro.
4. Por fim, realizamos um teste da função `soma-fatoriais`. O código solicita ao usuário que digite uma lista de números separados por espaço. Essa lista é convertida para uma lista de números utilizando as funções `read-line`, `string-split` e `map`. Em seguida, a função `soma-fatoriais` é chamada com a lista de números como parâmetro e o resultado é exibido na tela.

Espero que este código atenda às suas necessidades! Se tiver alguma dúvida ou precisar de mais informações, estou à disposição.