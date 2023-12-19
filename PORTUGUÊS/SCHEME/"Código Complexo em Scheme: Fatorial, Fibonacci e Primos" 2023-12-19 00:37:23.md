Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular o número de Fibonacci
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i 1) #t)
          ((= (modulo n i) 0) #f)
          (else (iter (- i 1)))))
  (iter (floor (sqrt n))))

;; Função para verificar se uma lista contém somente números primos
(define (somente-primos? lst)
  (cond ((null? lst) #t)
        ((not (primo? (car lst))) #f)
        (else (somente-primos? (cdr lst)))))

;; Função principal
(define (main)
  (display "Digite um número: ")
  (flush-output)
  (let ((numero (read)))
    (display "O fatorial do número é: ")
    (display (fatorial numero))
    (newline)
    (display "O número de Fibonacci é: ")
    (display (fibonacci numero))
    (newline)
    (display "A lista de números primos até o número é: ")
    (define lista-primos
      (filter primo? (range (+ 2 numero))))
    (display lista-primos)
    (newline)
    (display "A lista contém somente números primos? ")
    (display (somente-primos? lista-primos))
    (newline)))

(main)
```

Explicação do código:

- A função `fatorial` calcula o fatorial de um número usando recursão.
- A função `fibonacci` calcula o número de Fibonacci usando recursão.
- A função `primo?` verifica se um número é primo, usando uma função auxiliar `iter` que verifica se o número é divisível por algum número menor que ele.
- A função `somente-primos?` verifica se uma lista contém somente números primos, usando a função `primo?` e recursão.
- A função `main` é a função principal que solicita ao usuário um número, calcula o fatorial, o número de Fibonacci, gera uma lista de números primos até o número e verifica se a lista contém somente números primos. Os resultados são exibidos no console.

Espero que este código complexo em Scheme atenda às suas expectativas!