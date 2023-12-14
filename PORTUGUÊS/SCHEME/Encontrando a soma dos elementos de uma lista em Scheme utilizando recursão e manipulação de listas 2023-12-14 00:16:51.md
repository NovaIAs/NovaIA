Claro! Aqui está um código complexo em Scheme que envolve manipulação de listas e recursão:

```scheme
;; Função para encontrar a soma dos elementos de uma lista
(define (soma-lista lista)
  (cond ((null? lista) 0) ; caso base: lista vazia, retorna 0
        ((number? (car lista)) ; se o elemento atual é um número
         (+ (car lista) (soma-lista (cdr lista)))) ; soma o número ao resultado da chamada recursiva com o restante da lista
        ((list? (car lista)) ; se o elemento atual é uma sublista
         (+ (soma-lista (car lista)) (soma-lista (cdr lista)))) ; soma a soma dos elementos da sublista ao resultado da chamada recursiva com o restante da lista
        (else (soma-lista (cdr lista))))) ; caso contrário, chama recursivamente com o restante da lista

;; Exemplo de uso
(displayln (soma-lista '(1 2 3 (4 5) 6))) ; exibe 21
```

Neste código, temos a função `soma-lista` que recebe uma lista como parâmetro e retorna a soma de todos os elementos da lista, incluindo elementos que são sub-listas.

A função utiliza a recursão para percorrer a lista e somar os elementos. Ela utiliza a função `car` para obter o primeiro elemento da lista e a função `cdr` para obter o restante da lista após o primeiro elemento.

A função `cond` é utilizada para verificar diferentes casos. No primeiro caso, se a lista é vazia, ou seja, `null? lista`, a função retorna 0, pois não há elementos para somar.

Nos próximos casos, a função verifica se o elemento atual é um número (`number? (car lista)`) ou uma sublista (`list? (car lista)`).

Se o elemento atual for um número, a função soma o número atual (`(car lista)`) ao resultado da chamada recursiva `soma-lista` com o restante da lista (`(cdr lista)`).

Se o elemento atual for uma sublista, a função soma a soma dos elementos da sublista (`soma-lista (car lista)`) ao resultado da chamada recursiva `soma-lista` com o restante da lista (`(cdr lista)`).

Por fim, caso o elemento atual não seja um número nem uma sublista, a função chama recursivamente `soma-lista` com o restante da lista (`(cdr lista)`).

No exemplo de uso, temos a chamada `soma-lista '(1 2 3 (4 5) 6)`, que resulta na soma de todos os elementos da lista, incluindo a sublista `(4 5)`, resultando em 21.