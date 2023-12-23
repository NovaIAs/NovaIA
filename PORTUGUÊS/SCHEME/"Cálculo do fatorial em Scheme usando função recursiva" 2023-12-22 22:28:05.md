Claro! Aqui está um código em Scheme que implementa uma função recursiva para calcular o fatorial de um número:

```scheme
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

(display "Digite um número para calcular o fatorial: ")
(define num (read))
(display "O fatorial de ")
(display num)
(display " é ")
(display (fatorial num))
(newline)
```

Este código define uma função chamada `fatorial` que calcula o fatorial de um número `n`. A função utiliza recursão para calcular o fatorial, multiplicando `n` pelo fatorial do número anterior até chegar a 1.

Em seguida, o código solicita ao usuário para digitar um número e armazena esse número na variável `num`. Em seguida, ele exibe o resultado do cálculo do fatorial desse número.

Para executar esse código em Scheme, você precisa de um ambiente de execução Scheme, como o DrRacket. Basta copiar o código para um arquivo.rkt, executar e digitar o número desejado quando solicitado.

Espero que este exemplo seja útil para você! Se você tiver alguma dúvida ou precisar de mais ajuda, fique à vontade para perguntar.