Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (< n 2)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular o número de Fibonacci
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2))))))

;; Função para calcular o somatório dos quadrados dos números de 1 até n
(define (somatorio-quadrados n)
  (if (= n 0)
      0
      (+ (* n n) (somatorio-quadrados (- n 1)))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i n) #t)
          ((= (remainder n i) 0) #f)
          (else (iter (+ i 1)))))
  (iter 2))

;; Função para calcular a média de uma lista de números
(define (media lista)
  (/ (apply + lista) (length lista)))

;; Programa principal
(display "Calculadora Matemática")
(newline)
(display "------------------------")
(newline)

(let loop ()
  (display "Escolha uma opção:")
  (newline)
  (display "1. Calcular fatorial")
  (newline)
  (display "2. Calcular Fibonacci")
  (newline)
  (display "3. Calcular somatório dos quadrados")
  (newline)
  (display "4. Verificar se um número é primo")
  (newline)
  (display "5. Calcular média de uma lista de números")
  (newline)
  (display "6. Sair")
  (newline)
  (display "Opção: ")
  
  (let ((opcao (read)))
    (cond ((= opcao 1)
           (display "Digite um número para calcular o fatorial: ")
           (let ((num (read)))
             (display "O fatorial de ")
             (display num)
             (display " é ")
             (display (fatorial num))
             (newline)
             (newline)
             (loop))))
          
          ((= opcao 2)
           (display "Digite um número para calcular o número de Fibonacci: ")
           (let ((num (read)))
             (display "O número de Fibonacci de ")
             (display num)
             (display " é ")
             (display (fibonacci num))
             (newline)
             (newline)
             (loop))))
          
          ((= opcao 3)
           (display "Digite um número para calcular o somatório dos quadrados: ")
           (let ((num (read)))
             (display "O somatório dos quadrados até ")
             (display num)
             (display " é ")
             (display (somatorio-quadrados num))
             (newline)
             (newline)
             (loop))))
          
          ((= opcao 4)
           (display "Digite um número para verificar se é primo: ")
           (let ((num (read)))
             (display "O número ")
             (display num)
             (if (primo? num)
                 (display " é primo!")
                 (display " não é primo!"))
             (newline)
             (newline)
             (loop))))
          
          ((= opcao 5)
           (display "Digite uma lista de números separados por espaço: ")
           (let ((lista (map read (string-split (read-line) " "))))
             (display "A média da lista é ")
             (display (media lista))
             (newline)
             (newline)
             (loop))))
          
          ((= opcao 6)
           (display "Saindo...")
           (newline))
          
          (else
           (display "Opção inválida, tente novamente.")
           (newline)
           (newline)
           (loop))))))
```

Neste código em Scheme, criei uma calculadora matemática com várias funcionalidades, incluindo cálculo de fatorial, número de Fibonacci, somatório dos quadrados, verificação de números primos e cálculo da média de uma lista de números.

O programa principal apresenta um menu com opções numeradas de 1 a 6. O usuário pode escolher uma opção digitando o número correspondente. Dependendo da opção escolhida, o programa solicita os dados necessários (como um número para calcular o fatorial) e exibe o resultado na tela.

O código utiliza funções recursivas para calcular o fatorial e o número de Fibonacci. Também utiliza uma função auxiliar para verificar se um número é primo. A função `media` utiliza as funções `apply` e `length` para calcular a média de uma lista de números.

Espero que este código complexo em Scheme atenda às suas expectativas!