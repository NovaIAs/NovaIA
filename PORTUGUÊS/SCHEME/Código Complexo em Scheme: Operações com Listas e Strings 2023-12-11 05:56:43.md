Aqui está um exemplo de código complexo em Scheme:

```scheme
(define (soma-recursiva lista)
  (cond ((null? lista) 0)
        ((list? (car lista)) (+ (soma-recursiva (car lista)) (soma-recursiva (cdr lista))))
        (else (+ (car lista) (soma-recursiva (cdr lista))))))

(define (quicksort lista)
  (cond ((null? lista) '())
        ((null? (cdr lista)) lista)
        (else (let ((pivot (car lista))
                    (menores (filter (lambda (x) (< x pivot)) (cdr lista)))
                    (iguais (filter (lambda (x) (= x pivot)) (cdr lista)))
                    (maiores (filter (lambda (x) (> x pivot)) (cdr lista))))
                (append (quicksort menores) iguais (list pivot) (quicksort maiores))))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (valida-palindrome? str)
  (let ((clean-str (filter char-alphabetic? (string->list (string-downcase str)))))
    (equal? clean-str (reverse clean-str))))

(define (calcular-media lista)
  (let ((soma (soma-recursiva lista))
        (quantidade (length lista)))
    (/ soma quantidade)))

(define (converter-para-binario numero)
  (let loop ((n numero)
             (result '()))
    (if (zero? n)
        (if (null? result)
            '(0)
            result)
        (loop (quotient n 2) (cons (remainder n 2) result)))))

(define (verificar-palindromos lista-palavras)
  (map (lambda (palavra)
         (cons palavra (valida-palindrome? palavra)))
       lista-palavras))

(define (main)
  (display "Digite uma lista de números: ")
  (let ((lista (read)))
    (display "A soma dos números é: ")
    (display (soma-recursiva lista))
    (newline)
    (display "A lista ordenada é: ")
    (display (quicksort lista))
    (newline)
    (display "Digite um número para calcular o Fibonacci: ")
    (let ((n (read)))
      (display "O Fibonacci de ")
      (display n)
      (display " é: ")
      (display (fibonacci n)))
    (newline)
    (display "Digite uma palavra para verificar se é um palíndromo: ")
    (let ((palavra (read)))
      (display "A palavra ")
      (display palavra)
      (if (valida-palindrome? palavra)
          (display " é um palíndromo.")
          (display " não é um palíndromo.")))
    (newline)
    (display "Digite uma lista de números para calcular a média: ")
    (let ((lista-media (read)))
      (display "A média dos números é: ")
      (display (calcular-media lista-media)))
    (newline)
    (display "Digite um número para converter para binário: ")
    (let ((numero-binario (read)))
      (display "O número ")
      (display numero-binario)
      (display " em binário é: ")
      (display (converter-para-binario numero-binario))))
  (newline))

(main)
```

Esse código em Scheme realiza diversas operações:

1. `soma-recursiva`: função que recebe uma lista e retorna a soma de todos os números contidos nela, percorrendo a lista de forma recursiva.
2. `quicksort`: função que recebe uma lista e retorna uma nova lista ordenada em ordem crescente, utilizando o algoritmo de ordenação quicksort.
3. `fibonacci`: função que retorna o n-ésimo número da sequência de Fibonacci, utilizando recursão.
4. `valida-palindrome?`: função que verifica se uma string é um palíndromo, ignorando caracteres não alfabéticos e considerando letras maiúsculas e minúsculas como iguais.
5. `calcular-media`: função que recebe uma lista de números e retorna a média aritmética dos mesmos.
6. `converter-para-binario`: função que recebe um número inteiro e retorna uma lista contendo sua representação binária.
7. `verificar-palindromos`: função que recebe uma lista de palavras e retorna uma nova lista contendo cada palavra e um valor booleano indicando se é palíndromo ou não.
8. `main`: função principal que realiza a interação com o usuário, exibindo mensagens e lendo valores para executar as funções.

Esse código demonstra o uso de recursão, funções de ordem superior como `filter`, `map` e `lambda`, manipulação de strings e listas, e interação com o usuário.