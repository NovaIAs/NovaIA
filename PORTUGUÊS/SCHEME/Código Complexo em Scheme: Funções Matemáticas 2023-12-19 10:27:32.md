Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular o número de Fibonacci
(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Função para calcular o quadrado de um número
(define (quadrado n)
  (* n n))

;; Função para calcular o cosseno de um ângulo
(define (cosseno angulo)
  (define (iteracao x termo precisao)
    (if (< (abs termo) precisao)
        x
        (iteracao (+ x termo) (/ (* termo (- angulo (quadrado x))) (fatorial (+ x 2))) precisao)))
  (iteracao 0 1 0.0001))

;; Função para calcular a raiz quadrada de um número
(define (raiz-quadrada n)
  (define (iteracao x precisao)
    (if (< (abs (- (quadrado x) n)) precisao)
        x
        (iteracao (/ (+ x (/ n x)) 2) precisao)))
  (iteracao 1 0.0001))

;; Função principal
(define (main)
  (display "Digite o número para calcular seu fatorial: ")
  (define num-fatorial (read))
  (display "O fatorial de ")
  (display num-fatorial)
  (display " é ")
  (display (fatorial num-fatorial))
  (newline)

  (display "Digite o número para calcular seu valor na sequência de Fibonacci: ")
  (define num-fibonacci (read))
  (display "O número na posição ")
  (display num-fibonacci)
  (display " da sequência de Fibonacci é ")
  (display (fibonacci num-fibonacci))
  (newline)

  (display "Digite um ângulo em radianos para calcular o seu cosseno: ")
  (define angulo-cosseno (read))
  (display "O cosseno de ")
  (display angulo-cosseno)
  (display " é ")
  (display (cosseno angulo-cosseno))
  (newline)

  (display "Digite um número para calcular a sua raiz quadrada: ")
  (define num-raiz-quadrada (read))
  (display "A raiz quadrada de ")
  (display num-raiz-quadrada)
  (display " é ")
  (display (raiz-quadrada num-raiz-quadrada))
  (newline))

(main)
```

Este código em Scheme inclui várias funções matemáticas, como o cálculo do fatorial de um número, o cálculo do número na sequência de Fibonacci, o cálculo do quadrado de um número, o cálculo do cosseno de um ângulo e o cálculo da raiz quadrada de um número.

Na função principal (`main`), o código solicita a entrada do usuário para realizar esses cálculos. Em seguida, exibe os resultados na tela.

Espero que isso atenda às suas necessidades de um código complexo em Scheme!