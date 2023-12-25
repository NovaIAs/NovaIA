Claro! Aqui está um exemplo de código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular a sequência de Fibonacci
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (divisivel? d)
    (if (= d 1)
        #f
        (if (= (modulo n d) 0)
            #t
            (divisivel? (- d 1)))))
  (if (< n 2)
      #f
      (not (divisivel? (- n 1)))))

;; Função para calcular a média dos elementos de uma lista
(define (media lista)
  (/ (apply + lista) (length lista)))

;; Função para calcular o máximo divisor comum de dois números
(define (mdc a b)
  (if (= b 0)
      a
      (mdc b (modulo a b))))

;; Exemplo de uso das funções

(display "Digite um número para calcular o fatorial: ")
(define numero (read))
(display "O fatorial de ")
(display numero)
(display " é ")
(display (fatorial numero))
(newline)

(display "Digite um número para calcular a sequência de Fibonacci: ")
(define numero (read))
(display "A sequência de Fibonacci de ")
(display numero)
(display " números é ")
(display (map fibonacci (range numero)))
(newline)

(display "Digite um número para verificar se é primo: ")
(define numero (read))
(if (primo? numero)
    (display "O número é primo.")
    (display "O número não é primo."))
(newline)

(display "Digite a quantidade de elementos da lista: ")
(define tamanho (read))
(display "Digite os elementos da lista separados por espaço: ")
(define lista (map read (range tamanho)))
(display "A média dos elementos da lista é ")
(display (media lista))
(newline)

(display "Digite dois números para calcular o MDC: ")
(define a (read))
(define b (read))
(display "O MDC de ")
(display a)
(display " e ")
(display b)
(display " é ")
(display (mdc a b))
(newline))
```

Neste código, defini cinco funções diferentes:

1. `fatorial`: Calcula o fatorial de um número usando recursão.
2. `fibonacci`: Calcula a sequência de Fibonacci até o n-ésimo número.
3. `primo?`: Verifica se um número é primo.
4. `media`: Calcula a média dos elementos de uma lista.
5. `mdc`: Calcula o máximo divisor comum de dois números usando o algoritmo de Euclides.

Em seguida, o código faz uso das funções definidas, pedindo ao usuário para inserir números e exibindo os resultados calculados.