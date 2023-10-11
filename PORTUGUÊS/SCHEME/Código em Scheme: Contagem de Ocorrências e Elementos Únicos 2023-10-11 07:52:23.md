Claro! Aqui está um código complexo em Scheme que envolve a manipulação de listas e recursão:

```scheme
;; Função para contar o número de ocorrências de um elemento numa lista
(define (contar-ocorrencias elemento lista)
  (cond ((null? lista) 0)                        ; Se a lista estiver vazia, retorna 0
        ((eq? elemento (car lista))               ; Se o primeiro elemento da lista for igual ao elemento desejado
         (+ 1 (contar-ocorrencias elemento (cdr lista)))) ; incrementa 1 e chama a função recursivamente com o restante da lista
        (else (contar-ocorrencias elemento (cdr lista))))) ; caso contrário, chama a função recursivamente com o restante da lista

;; Função para criar uma lista com os elementos únicos de uma lista dada
(define (elementos-unicos lista)
  (define (iterar lista resultado)
    (cond ((null? lista) resultado)                  ; Se a lista estiver vazia, retorna o resultado acumulado
          ((memq (car lista) resultado)               ; Se o primeiro elemento da lista já está presente no resultado
           (iterar (cdr lista) resultado))            ; chama a função recursivamente sem adicionar o elemento
          (else (iterar (cdr lista) (cons (car lista) resultado))))) ; caso contrário, adiciona o elemento ao resultado e chama a função recursivamente

  (reverse (iterar lista '())))                     ; inverte a ordem dos elementos acumulados para obter a lista final

;; Exemplo de uso
(define lista-exemplo '(1 2 3 4 1 2 3 1 2 1))
(display "Lista exemplo: ")
(display lista-exemplo)
(newline)
(display "Elementos únicos na lista: ")
(display (elementos-unicos lista-exemplo))
(newline)
(display "Contagem de ocorrências do número 1 na lista: ")
(display (contar-ocorrencias 1 lista-exemplo))
(newline)
```

Neste código em Scheme, a função `contar-ocorrencias` recebe um elemento e uma lista como parâmetros e conta o número de ocorrências desse elemento na lista. A função percorre recursivamente a lista, comparando cada elemento com o elemento desejado, incrementando 1 na contagem sempre que houver uma correspondência.

A função `elementos-unicos` recebe uma lista como parâmetro e retorna uma nova lista contendo apenas os elementos únicos da lista recebida. A função utiliza uma função auxiliar `iterar` que percorre a lista original, acumulando os elementos únicos em uma lista resultado. A função `memq` é utilizada para verificar se o elemento já está presente no resultado acumulado.

No exemplo de uso, é criada uma lista exemplo `(1 2 3 4 1 2 3 1 2 1)`. Primeiro, é exibida a lista exemplo. Em seguida, é exibida a lista de elementos únicos na lista exemplo. Por fim, é exibida a contagem de ocorrências do número 1 na lista exemplo.