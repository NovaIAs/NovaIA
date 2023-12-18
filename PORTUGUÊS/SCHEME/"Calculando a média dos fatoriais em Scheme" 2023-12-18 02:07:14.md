
Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função recursiva para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Função auxiliar para calcular a soma dos fatoriais de uma lista de números
(define (soma-fatoriais lista)
  (cond ((null? lista) 0)
        ((not (pair? lista)) (fatorial lista))
        (else (+ (fatorial (car lista)) (soma-fatoriais (cdr lista))))))

;; Função principal para calcular a média dos fatoriais
(define (media-fatoriais lista)
  (let* ((soma (soma-fatoriais lista))
         (tamanho (length lista)))
    (/ soma tamanho)))

;; Exemplo de uso
(displayln "Digite uma lista de números:")
(define numeros (read))
(display "A média dos fatoriais é: ")
(display (media-fatoriais numeros))
(newline))
```

Este código em Scheme consiste em três funções: `fatorial`, `soma-fatoriais` e `media-fatoriais`.

A função `fatorial` é uma função recursiva que calcula o fatorial de um número. Ela verifica se o número é menor ou igual a 1 e, caso seja, retorna 1. Caso contrário, ela multiplica o número pelo fatorial do número anterior.

A função `soma-fatoriais` é uma função auxiliar que calcula a soma dos fatoriais de uma lista de números. Ela utiliza um condicional para tratar três casos: se a lista estiver vazia, retorna 0; se a lista contiver apenas um número, retorna o fatorial desse número; caso contrário, soma o fatorial do primeiro número da lista com a chamada recursiva da função para o restante da lista.

A função `media-fatoriais` é a função principal que calcula a média dos fatoriais de uma lista de números. Ela utiliza a função `soma-fatoriais` para calcular a soma dos fatoriais e a função `length` para obter o tamanho da lista. Em seguida, calcula a média dividindo a soma pelo tamanho.

No exemplo de uso, é solicitado ao usuário que digite uma lista de números. A lista é armazenada na variável `numeros` e, em seguida, é chamada a função `media-fatoriais` passando essa lista como argumento. O resultado é exibido na tela.

Espero que este código atenda às suas necessidades!