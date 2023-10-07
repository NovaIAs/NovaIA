Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Definindo uma função recursiva para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Definindo uma função para verificar se um número é primo
(define (primo? n)
  (define (divisivel? x)
    (if (= (remainder n x) 0)
        #t
        #f))
  
  (define (verificar-divisores x)
    (if (> x (/ n 2))
        #t
        (if (divisivel? x)
            #f
            (verificar-divisores (+ x 1)))))
  
  (if (<= n 1)
      #f
      (verificar-divisores 2)))

;; Definindo uma função para verificar se uma lista contém somente números primos
(define (lista-de-primos? lst)
  (define (verificar-elementos lst)
    (if (null? lst)
        #t
        (if (primo? (car lst))
            (verificar-elementos (cdr lst))
            #f)))
  
  (if (null? lst)
      #f
      (verificar-elementos lst)))

;; Utilizando as funções definidas anteriormente
(displayln "O fatorial de 5 é:")
(displayln (fatorial 5))

(displayln "Verificando se o número 7 é primo:")
(if (primo? 7)
    (displayln "É primo")
    (displayln "Não é primo"))

(displayln "Verificando se a lista '(2 3 5 7) contém somente números primos:")
(if (lista-de-primos? '(2 3 5 7))
    (displayln "Contém somente números primos")
    (displayln "Não contém somente números primos"))
```

Este código em Scheme é composto por três funções: `fatorial`, `primo?` e `lista-de-primos?`. 

A função `fatorial` é uma função recursiva que calcula o fatorial de um número. Ela utiliza uma condicional `if` para verificar se o número é igual a zero e, caso seja, retorna 1. Caso contrário, multiplica o número pelo resultado da chamada recursiva da função diminuindo o número em 1.

A função `primo?` verifica se um número é primo ou não. Ela utiliza duas funções auxiliares: `divisivel?` e `verificar-divisores`. A função `divisivel?` verifica se o número é divisível por um dado valor `x` através do operador `remainder`. A função `verificar-divisores` utiliza uma condicional `if` para verificar se o valor `x` é maior que metade do número. Se for, retorna `#t`. Caso contrário, verifica se o número é divisível por `x`. Se for, retorna `#f`. Caso contrário, chama recursivamente a função aumentando o valor de `x`. Por fim, a função `primo?` utiliza uma condicional `if` para verificar se o número é menor ou igual a 1. Se for, retorna `#f`. Caso contrário, chama a função `verificar-divisores` com o valor inicial de `x` igual a 2.

A função `lista-de-primos?` verifica se uma lista contém somente números primos. Ela utiliza uma função auxiliar `verificar-elementos` que utiliza uma condicional `if` para verificar se a lista está vazia. Se estiver, retorna `#t`. Caso contrário, verifica se o primeiro elemento da lista é primo. Se for, chama recursivamente a função com o restante da lista. Caso contrário, retorna `#f`. Por fim, a função `lista-de-primos?` utiliza uma condicional `if` para verificar se a lista está vazia. Se estiver, retorna `#f`. Caso contrário, chama a função `verificar-elementos` com a lista inicial.

No código final, há exemplos de utilização das funções definidas. Primeiro, calculamos o fatorial de 5 e imprimimos o resultado. Em seguida, verificamos se o número 7 é primo e imprimimos o resultado. Por fim, verificamos se a lista `(2 3 5 7)` contém somente números primos e imprimimos o resultado.