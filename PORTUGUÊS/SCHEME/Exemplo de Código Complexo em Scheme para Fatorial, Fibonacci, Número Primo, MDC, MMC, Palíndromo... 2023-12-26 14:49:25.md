Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular o número de Fibonacci
(define (fibonacci n)
  (if (<= n 2)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (auxiliar d)
    (cond ((= d n) true)
          ((= (remainder n d) 0) false)
          (else (auxiliar (+ d 1)))))
  (auxiliar 2))

;; Função para calcular o máximo divisor comum (MDC) de dois números
(define (mdc a b)
  (if (= b 0)
      a
      (mdc b (remainder a b))))

;; Função para calcular o mínimo múltiplo comum (MMC) de dois números
(define (mmc a b)
  (/ (* a b) (mdc a b)))

;; Função para verificar se uma string é um palíndromo
(define (palindromo? str)
  (define (limpar-string s)
    (define (auxiliar s resultado)
      (if (null? s)
          resultado
          (if (char-alphabetic? (car s))
              (auxiliar (cdr s) (cons (car s) resultado))
              (auxiliar (cdr s) resultado))))
    (auxiliar (string->list (string-downcase str)) '()))
  (let ((nova-string (limpar-string str)))
    (equal? nova-string (reverse nova-string))))

;; Função para calcular a raiz quadrada de um número
(define (raiz-quadrada n)
  (let ((tolerancia 0.0001)
        (aproximacao 1.0))
    (define (melhor-aproximacao)
      (/ (+ aproximacao (/ n aproximacao)) 2))
    (define (aproximacao-valida?)
      (< (abs (- (melhor-aproximacao) aproximacao))
         tolerancia))
    (define (iterar)
      (if (aproximacao-valida?)
          aproximacao
          (begin (set! aproximacao (melhor-aproximacao))
                 (iterar))))
    (iterar)))

;; Exemplo de utilização das funções
(displayln "O fatorial de 5 é:")
(displayln (fatorial 5))

(displayln "O 10º número de Fibonacci é:")
(displayln (fibonacci 10))

(displayln "O número 7 é primo?")
(displayln (primo? 7))

(displayln "O MDC de 24 e 36 é:")
(displayln (mdc 24 36))

(displayln "O MMC de 24 e 36 é:")
(displayln (mmc 24 36))

(displayln "A string 'somos' é um palíndromo?")
(displayln (palindromo? "somos"))

(displayln "A raiz quadrada de 16 é:")
(displayln (raiz-quadrada 16))
```

Neste código em Scheme, defini várias funções diferentes para realizar diferentes tarefas. Aqui está uma breve explicação de cada função:

- `fatorial`: Calcula o fatorial de um número usando recursão.
- `fibonacci`: Calcula o número de Fibonacci para um dado índice usando recursão.
- `primo?`: Verifica se um número é primo.
- `mdc`: Calcula o máximo divisor comum (MDC) de dois números usando o algoritmo de Euclides.
- `mmc`: Calcula o mínimo múltiplo comum (MMC) de dois números usando o MDC.
- `palindromo?`: Verifica se uma string é um palíndromo, removendo caracteres não alfabéticos e ignorando maiúsculas e minúsculas.
- `raiz-quadrada`: Calcula a raiz quadrada de um número usando o método de Newton.

No final do código, há exemplos de utilização de cada função para demonstrar seu funcionamento.