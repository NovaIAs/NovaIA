Claro! Aqui está um código complexo em Scheme que utiliza recursão e manipulação de listas para implementar uma função que verifica se uma lista de números está ordenada de forma crescente:

```scheme
(define (verificar-ordenacao lista)
  (cond ((null? lista) #t) ; Se a lista estiver vazia, está ordenada
        ((null? (cdr lista)) #t) ; Se a lista tiver apenas um elemento, está ordenada
        ((> (car lista) (cadr lista)) #f) ; Se o primeiro elemento for maior que o segundo, não está ordenada
        (else (verificar-ordenacao (cdr lista))))) ; Chama recursivamente a função para o restante da lista
```

Explicação do código:

1. Na primeira linha, definimos a função `verificar-ordenacao` que recebe uma lista como parâmetro.
2. Em seguida, utilizamos a construção `cond` para realizar uma série de testes e retornar um valor de acordo com o resultado de cada teste.
3. O primeiro teste verifica se a lista está vazia utilizando a função `null?`. Se a lista estiver vazia, retornamos `#t` (verdadeiro) indicando que está ordenada.
4. O segundo teste verifica se a lista possui apenas um elemento, utilizando a função `null?` em conjunto com `cdr` (que retorna a cauda da lista, ou seja, todos os elementos após o primeiro). Se a lista tiver apenas um elemento, retornamos `#t` indicando que está ordenada.
5. O terceiro teste verifica se o primeiro elemento da lista é maior que o segundo elemento, utilizando as funções `car` (que retorna o primeiro elemento da lista) e `cadr` (que retorna o segundo elemento da lista). Se o primeiro elemento for maior que o segundo, retornamos `#f` (falso) indicando que não está ordenada.
6. Caso nenhum dos testes anteriores seja verdadeiro, chamamos recursivamente a função `verificar-ordenacao` passando a cauda da lista como parâmetro, ou seja, todos os elementos após o primeiro. Isso permite que a função percorra toda a lista verificando se está ordenada.
7. O resultado final é o retorno da chamada recursiva da função para o restante da lista.

Esse código utiliza recursão para iterar sobre a lista, verificando se cada elemento é maior que o próximo. Se em algum momento encontrar uma inversão na ordem, retorna `#f` indicando que a lista não está ordenada. Caso contrário, retorna `#t` indicando que está ordenada.