Claro! Aqui está um código em Scheme que implementa a recursão de forma diferenciada. Ele é um exemplo de um programa que calcula o fatorial de um número de forma recursiva.

```scheme
;; Definindo a função fatorial
(define (fatorial n)
  (define (fatorial-iter resultado contador)
    (if (> contador n)
        resultado
        (fatorial-iter (* contador resultado) (+ contador 1))))
  (fatorial-iter 1 1))

;; Pedindo um número para o usuário
(display "Digite um número: ")
(define numero (read))

;; Chamando a função fatorial e exibindo o resultado
(display "O fatorial de ")
(display numero)
(display " é ")
(display (fatorial numero))
(newline)
```

Vamos entender o código passo a passo:

1. Definimos a função `fatorial` que recebe um número `n`.
2. Dentro de `fatorial`, definimos outra função chamada `fatorial-iter` que recebe dois parâmetros: `resultado` e `contador`.
3. Dentro de `fatorial-iter`, verificamos se `contador` é maior que `n`. Se for, retornamos o `resultado`; caso contrário, continuamos o cálculo do fatorial.
4. Em cada iteração, multiplicamos o `contador` pelo `resultado` atual e incrementamos o `contador` em 1.
5. Chamamos a função `fatorial-iter` com os valores iniciais `1` para `resultado` e `1` para `contador`.
6. Pedimos ao usuário para digitar um número e armazenamos o valor na variável `numero`.
7. Por fim, exibimos o resultado do cálculo do fatorial para o número digitado pelo usuário.

Esse código utiliza a recursão de cauda, que é uma forma especial de recursão em que a chamada recursiva é a última operação a ser executada dentro da função. Isso permite que a recursão seja otimizada pelo compilador ou interpretador, evitando o estouro da pilha de chamadas.

Espero que este exemplo atenda às suas expectativas! Se tiver mais alguma dúvida ou precisar de algum outro código, estou à disposição.