Certamente! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular a série de Fibonacci até o n-ésimo termo
(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i 1) #t)
          ((= (modulo n i) 0) #f)
          (else (iter (- i 1)))))
  (iter (floor (sqrt n))))

;; Função para calcular o somatório dos números primos até o n-ésimo termo
(define (somatorio_primos n)
  (define (iter i count sum)
    (cond ((= count n) sum)
          ((primo? i) (iter (+ i 1) (+ count 1) (+ sum i)))
          (else (iter (+ i 1) count sum))))
  (iter 2 0 0))

;; Função principal
(define (main n)
  (display "Digite a opção desejada:\n")
  (display "1 - Calcular o fatorial de um número\n")
  (display "2 - Calcular a série de Fibonacci até um número\n")
  (display "3 - Calcular o somatório dos números primos até um número\n")
  (let loop ()
    (display "Opção: ")
    (let ((opcao (read)))
      (cond ((= opcao 1)
             (display "Digite um número: ")
             (let ((numero (read)))
               (display "O fatorial de ")
               (display numero)
               (display " é ")
               (display (fatorial numero))
               (newline)
               (loop)))
            ((= opcao 2)
             (display "Digite um número: ")
             (let ((numero (read)))
               (display "A série de Fibonacci até ")
               (display numero)
               (display " é ")
               (display (fibonacci numero))
               (newline)
               (loop)))
            ((= opcao 3)
             (display "Digite um número: ")
             (let ((numero (read)))
               (display "O somatório dos números primos até ")
               (display numero)
               (display " é ")
               (display (somatorio_primos numero))
               (newline)
               (loop)))
            (else
             (display "Opção inválida. Tente novamente.\n")
             (loop))))))

(main 0)
```

Neste código em Scheme, implementei diferentes funções matemáticas. A função `fatorial` calcula o fatorial de um número, a função `fibonacci` calcula a série de Fibonacci até o n-ésimo termo, a função `primo?` verifica se um número é primo e a função `somatorio_primos` calcula o somatório dos números primos até o n-ésimo termo.

A função principal `main` exibe um menu com três opções: calcular o fatorial de um número, calcular a série de Fibonacci até um número e calcular o somatório dos números primos até um número. O usuário pode escolher uma opção digitando o número correspondente. Em seguida, o programa solicita o número necessário para realizar o cálculo e exibe o resultado.

Espero que isso tenha atendido às suas expectativas! Se você tiver mais alguma solicitação, fique à vontade para perguntar.