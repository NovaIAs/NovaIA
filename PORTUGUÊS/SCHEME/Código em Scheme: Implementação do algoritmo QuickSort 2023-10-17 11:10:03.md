Claro! Aqui está um código em Scheme que implementa o algoritmo de ordenação QuickSort. O QuickSort é um algoritmo de ordenação eficiente que divide a lista em partes menores e as ordena separadamente. Ele é amplamente utilizado devido à sua velocidade e eficiência.

```scheme
; Função auxiliar para trocar dois elementos na lista
(define (trocar lista i j)
  (let ((temp (list-ref lista i)))
    (list-set! lista i (list-ref lista j))
    (list-set! lista j temp)))

; Função para particionar a lista em torno de um pivô
(define (particionar lista inicio fim)
  (let ((pivot (list-ref lista fim))
        (i inicio)
        (j inicio))
    (while (< j fim)
      (if (< (list-ref lista j) pivot)
          (begin
            (trocar lista i j)
            (set! i (+ i 1))))
      (set! j (+ j 1)))
    (trocar lista i fim)
    i))

; Função principal do QuickSort
(define (quicksort lista inicio fim)
  (if (< inicio fim)
      (let ((piv (particionar lista inicio fim)))
        (quicksort lista inicio (- piv 1))
        (quicksort lista (+ piv 1) fim))))

; Função de interface para chamar o QuickSort
(define (ordena lista)
  (quicksort lista 0 (- (length lista) 1))
  lista)

; Exemplo de uso
(displayln "Digite uma lista de números separados por espaço:")
(define input (read-line))
(define lista (map string->number (string-split input)))
(displayln "Lista original:")
(displayln lista)
(displayln "Lista ordenada:")
(displayln (ordena lista))
```

Explicação do código:

1. A função `trocar` é uma função auxiliar que troca dois elementos na lista. Ela recebe a lista e os índices dos elementos a serem trocados.

2. A função `particionar` é responsável por particionar a lista em torno de um pivô. Ela recebe a lista, o índice inicial e o índice final. Ela seleciona o último elemento da lista como pivô e rearranja os elementos menores à esquerda e os maiores à direita do pivô.

3. A função `quicksort` é a função principal do algoritmo QuickSort. Ela recebe a lista, o índice inicial e o índice final. Ela verifica se a lista contém mais de um elemento e, se sim, chama a função `particionar` para dividir a lista em partes menores. Em seguida, chama recursivamente o `quicksort` para ordenar as partes menores.

4. A função `ordena` é uma função de interface que chama o `quicksort` e retorna a lista ordenada.

5. No exemplo de uso, o código solicita ao usuário para digitar uma lista de números separados por espaço. Ele converte a entrada em uma lista de números e chama a função `ordena` para ordenar a lista. Em seguida, exibe a lista original e a lista ordenada.

Espero que este código atenda às suas necessidades! Se você tiver alguma dúvida ou precisar de mais informações, sinta-se à vontade para perguntar.