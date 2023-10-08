Claro! Aqui está um exemplo de código em Scheme um pouco complexo que implementa a ordenação por inserção em uma lista:

```scheme
(define (inserir-ordenado lista elemento)
  (cond ((null? lista) (list elemento))
        ((<= (car lista) elemento) (cons (car lista) (inserir-ordenado (cdr lista) elemento)))
        (else (cons elemento lista))))

(define (ordenacao-insercao lista)
  (define (iterar lista resultado)
    (if (null? lista)
        resultado
        (iterar (cdr lista) (inserir-ordenado resultado (car lista)))))

  (iterar (cdr lista) (list (car lista))))

(define (imprimir-lista lista)
  (if (null? lista)
      (display "Fim da lista")
      (begin
        (display (car lista))
        (display " ")
        (imprimir-lista (cdr lista)))))

(let ((lista '(3 1 4 1 5 9 2 6 5 3 5)))
  (display "Lista original: ")
  (imprimir-lista lista)
  (newline)
  (set! lista (ordenacao-insercao lista))
  (display "Lista ordenada: ")
  (imprimir-lista lista))
```

Neste código, há três funções principais: `inserir-ordenado`, `ordenacao-insercao` e `imprimir-lista`.

A função `inserir-ordenado` recebe uma lista ordenada e um elemento a ser inserido, e retorna uma nova lista com o elemento inserido no local adequado. Ela utiliza recursão e a função `cons` para construir a nova lista com o elemento no local correto.

A função `ordenacao-insercao` é a função principal que implementa a ordenação por inserção em uma lista. Ela chama a função auxiliar `iterar`, que percorre a lista original e, a cada iteração, insere o elemento na lista resultado utilizando a função `inserir-ordenado`. No final, ela retorna a lista ordenada.

A função `imprimir-lista` é uma função auxiliar que imprime os elementos da lista um a um, separados por espaço.

Por fim, o código cria uma lista de exemplo `(3 1 4 1 5 9 2 6 5 3 5)`, imprime a lista original, chama a função de ordenação e imprime a lista ordenada.

Espero que este exemplo seja útil e demonstre como implementar a ordenação por inserção em Scheme!