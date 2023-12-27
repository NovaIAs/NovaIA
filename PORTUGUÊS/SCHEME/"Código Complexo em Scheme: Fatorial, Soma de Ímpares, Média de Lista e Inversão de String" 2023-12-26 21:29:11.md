Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

(define (soma-impares n)
  (if (<= n 0)
      0
      (if (odd? n)
          (+ n (soma-impares (- n 1)))
          (soma-impares (- n 1)))))

(define (media-lista lista)
  (letrec ((iter (lambda (lst count sum)
                   (if (null? lst)
                       (/ sum count)
                       (iter (cdr lst) (+ count 1) (+ sum (car lst)))))))
    (iter lista 0 0)))

(define (maior-elemento lista)
  (letrec ((iter (lambda (lst max)
                   (if (null? lst)
                       max
                       (iter (cdr lst) (if (> (car lst) max) (car lst) max))))))
    (iter lista (car lista))))

(define (inverter-string str)
  (letrec ((iter (lambda (lst acc)
                   (if (null? lst)
                       acc
                       (iter (cdr lst) (cons (car lst) acc))))))
    (list->string (iter (string->list str) '()))))

(define (fatorial-soma-media-inverter n lista str)
  (let ((fat (fatorial n))
        (soma (soma-impares n))
        (media (media-lista lista))
        (invertida (inverter-string str)))
    (displayln (format "Fatorial de ~a: ~a" n fat))
    (displayln (format "Soma dos ímpares até ~a: ~a" n soma))
    (displayln (format "Média da lista: ~a" media))
    (displayln (format "String invertida: ~a" invertida))))

(fatorial-soma-media-inverter 5 '(1 2 3 4 5) "Hello, World!")
```

Explicação do código:

1. A função `fatorial` recebe um número `n` e retorna o fatorial desse número. Ela utiliza uma recursão para calcular o fatorial.

2. A função `soma-impares` recebe um número `n` e retorna a soma de todos os números ímpares até `n`. Ela também utiliza recursão para fazer a soma.

3. A função `media-lista` recebe uma lista de números e retorna a média desses números. Ela utiliza uma função aninhada `iter` para iterar pela lista e calcular a média.

4. A função `maior-elemento` recebe uma lista de números e retorna o maior elemento dessa lista. Ela também utiliza uma função aninhada `iter` para iterar pela lista e encontrar o maior elemento.

5. A função `inverter-string` recebe uma string e retorna a string invertida. Ela utiliza uma função aninhada `iter` para iterar pela string e construir uma nova string invertida.

6. A função `fatorial-soma-media-inverter` recebe um número inteiro `n`, uma lista de números e uma string. Ela calcula o fatorial de `n`, a soma dos números ímpares até `n`, a média da lista e inverte a string. Em seguida, exibe essas informações na saída padrão.

No exemplo final, `fatorial-soma-media-inverter` é chamada com `n` igual a 5, uma lista contendo os números de 1 a 5 e a string "Hello, World!". Os resultados são impressos na tela.