; Este é um exemplo de um código complexo em Scheme.
; Ele implementa uma função recursiva chamada fatorial.
; O fatorial de um número é o produto de todos os números inteiros de 1 até o próprio número.

(define (fatorial n)
  ; Verifica se o número é igual a 0 ou 1.
  ; Neste caso, retorna 1, pois o fatorial de 0 e 1 é 1.
  (cond ((or (= n 0) (= n 1)) 1)
        
        ; Caso contrário, chama recursivamente a função fatorial
        ; com o argumento decrementado em 1 e multiplica o resultado
        ; pelo próprio número.
        (else (* n (fatorial (- n 1)))))


; Exemplos de uso da função fatorial:
(displayln (fatorial 5)) ; Saída: 120
(displayln (fatorial 10)) ; Saída: 3628800
(displayln (fatorial 0)) ; Saída: 1
(displayln (fatorial 1)) ; Saída: 1

; Outra função para calcular o fatorial de forma iterativa:

(define (fatorial-iterativo n)
  ; Define uma função auxiliar que recebe três parâmetros:
  ; o número para calcular o fatorial, o resultado parcial
  ; e um contador inicializado com 1.
  (define (iterar num resultado contador)
    
    ; Verifica se o contador é maior que o número.
    ; Neste caso, retorna o resultado parcial.
    (if (> contador num)
        resultado
        
        ; Caso contrário, chama recursivamente a função auxiliar
        ; com o número, o resultado parcial multiplicado pelo contador
        ; e o contador incrementado em 1.
        (iterar num (* resultado contador) (+ contador 1))))
  
  ; Chama a função auxiliar com o número, resultado parcial como 1 e contador como 1.
  (iterar n 1 1))


; Exemplos de uso da função fatorial-iterativo:
(displayln (fatorial-iterativo 5)) ; Saída: 120
(displayln (fatorial-iterativo 10)) ; Saída: 3628800
(displayln (fatorial-iterativo 0)) ; Saída: 1
(displayln (fatorial-iterativo 1)) ; Saída: 1