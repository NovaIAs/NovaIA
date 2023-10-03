Claro! Aqui está um exemplo de código em Scheme que implementa um algoritmo de busca binária recursiva:

```scheme
(define (busca-binaria-recursiva lista elemento inicio fim)
  (if (> inicio fim)
      #f
      (let* ((meio (quotient (+ inicio fim) 2))
             (valor-meio (list-ref lista meio)))
        (cond ((= valor-meio elemento) meio)
              ((< valor-meio elemento)
               (busca-binaria-recursiva lista elemento (+ meio 1) fim))
              ((> valor-meio elemento)
               (busca-binaria-recursiva lista elemento inicio (- meio 1)))))))
```

A função `busca-binaria-recursiva` recebe quatro parâmetros: `lista`, a lista ordenada onde será realizada a busca; `elemento`, o elemento que estamos procurando na lista; `inicio`, o índice de início da busca na lista; e `fim`, o índice de fim da busca na lista.

A função utiliza recursão para dividir a lista ao meio e comparar o elemento do meio com o elemento que estamos buscando. Se forem iguais, a função retorna o índice do elemento encontrado. Caso contrário, se o valor do meio for menor que o elemento buscado, a função chama a si mesma novamente, passando o índice de início como `meio + 1` e o índice de fim permanecendo o mesmo. Se o valor do meio for maior que o elemento buscado, a função chama a si mesma novamente, passando o índice de início permanecendo o mesmo e o índice de fim como `meio - 1`. Esse processo é repetido até que o elemento seja encontrado ou até que a busca seja esgotada (quando `inicio` se torna maior que `fim`).

Caso o elemento seja encontrado, a função retorna o índice do elemento na lista. Caso contrário, ela retorna `#f` para indicar que o elemento não está presente na lista.

Essa implementação utiliza o conceito de divisão e conquista, reduzindo o espaço de busca pela metade a cada iteração, resultando em uma busca eficiente em listas ordenadas.