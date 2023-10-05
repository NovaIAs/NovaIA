Claro, vou criar um código complexo em Scheme para você. Neste código, vamos implementar um algoritmo de busca em largura para encontrar o caminho mais curto em um labirinto representado por uma matriz.

Primeiro, vamos definir uma função chamada `encontrar-caminho` que recebe como parâmetros o labirinto e as coordenadas de início e fim. Essa função irá retornar uma lista com as coordenadas que representam o caminho mais curto.

```scheme
(define (encontrar-caminho labirinto inicio fim)
  (define (valido? x y)
    (and (>= x 0) (< x (length labirinto))
         (>= y 0) (< y (length (car labirinto)))
         (not (equal? (list x y) inicio))
         (equal? (list-ref (list-ref labirinto x) y) 0)))
  
  (define (vizinhos x y)
    (list (list (+ x 1) y) (list (- x 1) y)
          (list x (+ y 1)) (list x (- y 1))))
  
  (define (bfs fila visitados)
    (cond ((null? fila) '()) ; Se a fila estiver vazia, não há mais caminhos possíveis
          ((equal? (car fila) fim) (reverse (car fila))) ; Se encontramos o destino, retorna o caminho
          (else
           (let ((atual (car fila))
                 (resto (cdr fila))
                 (x (car (car fila)))
                 (y (cadr (car fila))))
             (bfs (append resto (filter (lambda (viz) (valido? (car viz) (cadr viz)))
                                        (vizinhos x y)))
                  (cons atual visitados)))))) ; Adiciona o nó atual à lista de visitados
  
  (bfs (list inicio) '()))
```

A função `valido?` é responsável por verificar se uma dada coordenada `(x, y)` é válida no labirinto. Ela verifica se as coordenadas estão dentro dos limites da matriz, se não é a coordenada de início e se o valor na posição `(x, y)` é igual a 0, indicando que é possível passar por ali.

A função `vizinhos` retorna uma lista com as coordenadas dos vizinhos adjacentes a uma dada coordenada `(x, y)`. São considerados vizinhos as posições acima, abaixo, à esquerda e à direita.

A função `bfs` implementa o algoritmo de busca em largura. Ela recebe uma fila de coordenadas a serem exploradas e uma lista de coordenadas já visitadas. O algoritmo continua explorando coordenadas até encontrar o destino ou até não haver mais caminhos possíveis. Ele utiliza a fila para armazenar os nós a serem explorados e a lista de visitados para evitar ciclos.

Por fim, a função `encontrar-caminho` chama a função `bfs` passando a fila inicial contendo apenas a coordenada de início e a lista de visitados vazia. Ela retorna o caminho mais curto encontrado.

Esse código implementa um algoritmo de busca em largura eficiente para encontrar o caminho mais curto em um labirinto representado por uma matriz.