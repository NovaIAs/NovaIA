Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função para checar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i n) #t)
          ((= (modulo n i) 0) #f)
          (else (iter (+ i 1)))))
  (iter 2))

;; Função para encontrar os números primos menores que um dado limite
(define (encontrar-primos limite)
  (define (iter n primos)
    (cond ((<= n limite)
           (if (primo? n)
               (iter (+ n 1) (cons n primos))
               (iter (+ n 1) primos)))
          (else primos)))
  (iter 2 '()))

;; Função para calcular a soma dos fatoriais dos números primos menores que um dado limite
(define (soma-fatoriais-primos limite)
  (define primos (encontrar-primos limite))
  (define (iter primos soma)
    (cond ((null? primos) soma)
          (else (iter (cdr primos) (+ soma (fatorial (car primos)))))))
  (iter primos 0))

;; Exemplo de uso
(displayln "Digite um limite:")
(define limite (read))
(displayln (format "A soma dos fatoriais dos números primos menores que ~a é ~a" limite (soma-fatoriais-primos limite)))
```

Neste código, temos várias funções em Scheme que realizam diferentes tarefas.

A função `fatorial` calcula o fatorial de um número usando uma recursão simples. Ela verifica se o número é menor ou igual a 1 e, caso seja, retorna 1. Caso contrário, multiplica o número pela chamada recursiva da função para o número decrementado em 1.

A função `primo?` verifica se um número é primo. Ela utiliza a função interna `iter` para iterar de 2 até o número em questão. A cada iteração, a função verifica se o número é igual ao iterador (indicando que é primo) ou se o número é divisível pelo iterador (indicando que não é primo). Caso nenhuma das condições seja satisfeita, ela chama recursivamente a função `iter` com o iterador incrementado em 1.

A função `encontrar-primos` encontra todos os números primos menores que um dado limite. Ela utiliza a função interna `iter` para iterar de 2 até o limite. A cada iteração, a função verifica se o número é primo usando a função `primo?`. Caso seja, adiciona o número à lista de primos. Caso contrário, apenas continua a iteração. No final, retorna a lista de primos encontrados.

A função `soma-fatoriais-primos` calcula a soma dos fatoriais dos números primos menores que um dado limite. Ela utiliza a função `encontrar-primos` para obter a lista de primos e, em seguida, utiliza a função interna `iter` para iterar sobre essa lista. A cada iteração, a função calcula o fatorial do número atual e adiciona o resultado à soma. No final, retorna a soma dos fatoriais.

Por fim, o exemplo de uso solicita ao usuário que digite um limite e, em seguida, chama a função `soma-fatoriais-primos` passando esse limite como argumento. O resultado é então exibido na tela.