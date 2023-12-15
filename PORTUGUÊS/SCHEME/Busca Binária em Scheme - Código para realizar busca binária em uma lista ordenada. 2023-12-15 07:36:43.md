Certamente! Aqui está um código em Scheme que realiza uma busca binária em uma lista ordenada. 

```scheme
(define (busca-binaria lista valor)
  (letrec ((busca (lambda (inicio fim)
                    (cond ((null? lista) #f)
                          ((= inicio fim)
                           (if (= (car lista) valor)
                               #t
                               #f))
                          (else
                           (let* ((meio (quotient (+ inicio fim) 2))
                                  (elemento (list-ref lista meio)))
                             (cond ((= elemento valor) #t)
                                   ((> elemento valor)
                                    (busca inicio (- meio 1)))
                                   (else
                                    (busca (+ meio 1) fim)))))))))
    (busca 0 (- (length lista) 1))))
```

Neste código, temos uma função chamada `busca-binaria` que recebe uma lista ordenada `lista` e um valor a ser buscado `valor`. O algoritmo utiliza uma abordagem recursiva para realizar a busca binária na lista.

A função `busca` é definida dentro da função principal, utilizando `letrec`. Ela recebe dois parâmetros: `inicio` e `fim`, que indicam o intervalo da lista onde a busca binária será realizada.

Dentro da função `busca`, são verificados vários casos:
- Se a lista estiver vazia, a função retorna `#f`, indicando que o valor não foi encontrado.
- Se `inicio` for igual a `fim`, significa que chegamos a um intervalo de tamanho 1. Neste caso, comparamos o elemento da lista com o valor buscado. Se forem iguais, retornamos `#t`, caso contrário, retornamos `#f`.
- Caso contrário, calculamos o valor do meio do intervalo utilizando a função `quotient` para evitar números decimais. Em seguida, obtemos o elemento correspondente ao meio da lista utilizando `list-ref`.
- Comparamos o elemento encontrado com o valor buscado. Se forem iguais, retornamos `#t`.
- Se o elemento for maior que o valor buscado, chamamos recursivamente a função `busca` passando `inicio` e `(menos meio 1)` como parâmetros.
- Se o elemento for menor que o valor buscado, chamamos recursivamente a função `busca` passando `(mais meio 1)` e `fim` como parâmetros.

Dessa forma, o código realiza a busca binária de forma eficiente, dividindo o intervalo de busca pela metade a cada iteração, até encontrar o valor desejado ou determinar que ele não está presente na lista.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, é só me dizer.