Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para verificar se um número é primo
(define (primo? n)
  (define (divisivel? x y)
    (= (modulo x y) 0))
  
  (define (verificar-divisores x divisor)
    (if (= divisor 1)
        #t
        (if (divisivel? x divisor)
            #f
            (verificar-divisores x (- divisor 1)))))

  (if (< n 2)
      #f
      (verificar-divisores n (- (sqrt n) 1))))

;; Função para gerar uma lista com os números primos menores que n
(define (gerar-primos n)
  (define (gerar-primos-aux i)
    (if (>= i n)
        '()
        (if (primo? i)
            (cons i (gerar-primos-aux (+ i 1)))
            (gerar-primos-aux (+ i 1)))))
  
  (gerar-primos-aux 2))

;; Função para verificar se um número é perfeito
(define (perfeito? n)
  (define (somar-divisores x divisor soma)
    (if (= divisor 0)
        soma
        (if (divisivel? x divisor)
            (somar-divisores x (- divisor 1) (+ soma divisor))
            (somar-divisores x (- divisor 1) soma))))
  
  (= (somar-divisores n (- n 1) 0) n))

;; Função principal para gerar uma lista com os números perfeitos menores que n
(define (gerar-perfeitos n)
  (define (gerar-perfeitos-aux i)
    (if (>= i n)
        '()
        (if (perfeito? i)
            (cons i (gerar-perfeitos-aux (+ i 1)))
            (gerar-perfeitos-aux (+ i 1)))))
  
  (gerar-perfeitos-aux 2))

;; Exemplo de uso das funções
(display "Digite um número inteiro positivo: ")
(let ((n (string->number (read-line))))
  (display "Números primos menores que ")
  (display n)
  (display ": ")
  (display (gerar-primos n))
  (newline)
  (display "Números perfeitos menores que ")
  (display n)
  (display ": ")
  (display (gerar-perfeitos n))
  (newline))
```

Explicação do código:

1. Definimos a função `fatorial`, que calcula o fatorial de um número utilizando recursão. A função verifica se o número é menor ou igual a 1 e retorna 1, caso contrário, multiplica o número pelo fatorial do número anterior.
2. Definimos a função `primo?`, que verifica se um número é primo. A função utiliza duas funções auxiliares: `divisivel?`, que verifica se um número é divisível por outro, e `verificar-divisores`, que verifica se um número é divisível por algum número menor que ele. A função `primo?` primeiro verifica se o número é menor que 2, retornando `#f` nesse caso. Caso contrário, utiliza a função `verificar-divisores` para verificar se o número é divisível por algum número menor que a raiz quadrada do número.
3. Definimos a função `gerar-primos`, que gera uma lista com os números primos menores que um dado número. A função utiliza a função auxiliar `gerar-primos-aux`, que recebe um número e verifica se ele é primo. Caso seja primo, o número é adicionado à lista, caso contrário, o próximo número é verificado. A função principal chama a função auxiliar com o número inicial 2.
4. Definimos a função `perfeito?`, que verifica se um número é perfeito. A função utiliza a função auxiliar `somar-divisores`, que recebe um número, um divisor e uma soma. A função verifica se o divisor é igual a 0 e retorna a soma, caso seja. Caso contrário, verifica se o número é divisível pelo divisor. Se for, a função chama recursivamente `somar-divisores` com o divisor anterior decrementado e a soma atualizada com o valor do divisor. Se não for divisível, a função chama recursivamente `somar-divisores` com o divisor anterior decrementado e a soma atual. A função `perfeito?` verifica se a soma dos divisores é igual ao número.
5. Definimos a função `gerar-perfeitos`, que gera uma lista com os números perfeitos menores que um dado número. A função utiliza a função auxiliar `gerar-perfeitos-aux`, que recebe um número e verifica se ele é perfeito. Caso seja perfeito, o número é adicionado à lista, caso contrário, o próximo número é verificado. A função principal chama a função auxiliar com o número inicial 2.
6. Por fim, temos um exemplo de uso das funções, onde o usuário digita um número inteiro positivo e o programa exibe a lista de números primos e perfeitos menores que o número digitado.