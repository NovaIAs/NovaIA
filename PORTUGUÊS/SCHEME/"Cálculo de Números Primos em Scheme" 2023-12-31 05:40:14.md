Claro! Aqui está um código complexo em Scheme que envolve o cálculo de números primos e a manipulação de listas:

```scheme
;; Função auxiliar para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i 1) #t)
          ((= (modulo n i) 0) #f)
          (else (iter (- i 1)))))
  (iter (- n 1)))

;; Função para retornar uma lista de números primos menores ou iguais a n
(define (lista-primos n)
  (define (iter i lst)
    (cond ((= i 1) lst)
          ((primo? i) (iter (- i 1) (cons i lst)))
          (else (iter (- i 1) lst))))
  (iter n '()))

;; Função auxiliar para verificar se um número é múltiplo de outro
(define (multiplo? a b)
  (= (modulo a b) 0))

;; Função para remover os múltiplos de um número de uma lista
(define (remover-multiplos lst n)
  (cond ((null? lst) '())
        ((multiplo? (car lst) n) (remover-multiplos (cdr lst) n))
        (else (cons (car lst) (remover-multiplos (cdr lst) n)))))

;; Função para gerar uma lista de primos menores ou iguais a n
(define (gerar-lista-primos n)
  (define (iter lst)
    (cond ((null? lst) '())
          (else (cons (car lst) (iter (remover-multiplos (cdr lst) (car lst)))))))
  (iter (lista-primos n)))

;; Exemplo de uso
(displayln "Digite um número:")
(define n (read))
(display "Lista de números primos menores ou iguais a ")
(display n)
(display ": ")
(display (gerar-lista-primos n))
(newline))
```

Neste código, começamos definindo uma função auxiliar `primo?` que verifica se um número é primo. A função `lista-primos` retorna uma lista de números primos menores ou iguais a um número `n`. Em seguida, temos a função auxiliar `multiplo?`, que verifica se um número é múltiplo de outro.

A função `remover-multiplos` remove os múltiplos de um número de uma lista. Por fim, temos a função `gerar-lista-primos`, que recebe um número `n` e retorna a lista de primos menores ou iguais a `n`.

No exemplo de uso, o usuário é solicitado a digitar um número. Em seguida, é exibida a lista de números primos menores ou iguais ao número digitado.

Espero que este código atenda às suas expectativas!