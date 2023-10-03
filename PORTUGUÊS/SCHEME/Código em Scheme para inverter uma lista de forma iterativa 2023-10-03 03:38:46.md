Claro! Aqui está um código complexo em Scheme que envolve o uso de recursão e manipulação de listas. O código implementa uma função que inverte uma lista de forma iterativa, ou seja, sem o uso de recursão direta.

```scheme
(define (inverter-lista lista)
  (define (inverter-lista-iterativa lista resultado)
    (if (null? lista)
        resultado
        (inverter-lista-iterativa (cdr lista) (cons (car lista) resultado))))
  (inverter-lista-iterativa lista '()))

(displayln (inverter-lista '(1 2 3 4 5)))
```

A função `inverter-lista` recebe uma lista como entrada e chama uma função auxiliar `inverter-lista-iterativa`, que realiza a inversão de forma iterativa. A função `inverter-lista-iterativa` recebe dois parâmetros: `lista`, que representa a lista original que deve ser invertida, e `resultado`, que é uma lista vazia que será construída com os elementos invertidos.

A função `inverter-lista-iterativa` utiliza um condicional `if` para verificar se a `lista` é vazia. Se for, significa que a inversão está completa e o `resultado` é retornado. Caso contrário, a função chama a si mesma recursivamente, passando a `lista` sem o seu primeiro elemento (obtido através da função `cdr`) e o `resultado` atualizado com o primeiro elemento da `lista` (obtido através da função `car`) adicionado no início (obtido através da função `cons`).

Finalmente, o resultado da inversão da lista é exibido na tela através da função `displayln`.

Espero que esse código atenda às suas expectativas! Se você tiver mais alguma solicitação ou precisar de ajuda com outro código, é só me pedir.