Claro! Aqui está um código complexo em Scheme que envolve o uso de funções recursivas e manipulação de listas.

```scheme
;; Função para encontrar o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular a soma dos fatoriais dos números de uma lista
(define (soma-fatoriais lista)
  (cond ((null? lista) 0)
        ((not (number? (car lista))) (soma-fatoriais (cdr lista)))
        (else (+ (fatorial (car lista)) (soma-fatoriais (cdr lista))))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iterador divisor)
    (cond ((< divisor 2) #t)
          ((zero? (remainder n divisor)) #f)
          (else (iterador (- divisor 1)))))
  (if (< n 2)
      #f
      (iterador (- n 1))))

;; Função para gerar uma lista de números primos até um número limite
(define (gerar-primos limite)
  (define (iterador n lista)
    (if (< n 2)
        lista
        (if (primo? n)
            (iterador (- n 1) (cons n lista))
            (iterador (- n 1) lista))))
  (iterador limite '()))

;; Função para somar os números primos de uma lista
(define (soma-primos lista)
  (cond ((null? lista) 0)
        ((not (number? (car lista))) (soma-primos (cdr lista)))
        (else (if (primo? (car lista))
                  (+ (car lista) (soma-primos (cdr lista)))
                  (soma-primos (cdr lista))))))

;; Função principal que utiliza todas as funções anteriores
(define (calcular lista)
  (define soma-fatoriais (soma-fatoriais lista))
  (define soma-primos (soma-primos lista))
  (display "A soma dos fatoriais dos números da lista é: ")
  (display soma-fatoriais)
  (newline)
  (display "A soma dos números primos da lista é: ")
  (display soma-primos)
  (newline)
  (+ soma-fatoriais soma-primos))
```

Neste código em Scheme, temos várias definições de funções. A função `fatorial` recebe um número `n` e retorna o fatorial desse número. A função `soma-fatoriais` recebe uma lista de números e calcula a soma dos fatoriais de todos os números da lista. A função `primo?` verifica se um número é primo ou não. A função `gerar-primos` gera uma lista de números primos até um determinado limite. A função `soma-primos` recebe uma lista de números e calcula a soma dos números primos dessa lista. Por fim, a função `calcular` utiliza todas as funções anteriores para calcular e exibir a soma dos fatoriais e a soma dos números primos de uma determinada lista.