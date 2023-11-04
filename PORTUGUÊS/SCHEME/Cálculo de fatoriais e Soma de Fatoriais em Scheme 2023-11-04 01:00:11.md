Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0)
      1
      (* n (fatorial (- n 1)))))

;; Função para calcular a soma dos fatoriais de uma lista de números
(define (soma-fatoriais lista)
  (if (null? lista)
      0
      (+ (fatorial (car lista))
         (soma-fatoriais (cdr lista)))))

;; Função para encontrar o maior fatorial em uma lista de números
(define (maior-fatorial lista)
  (define (iterar lista maior)
    (if (null? lista)
        maior
        (let ((fatorial-atual (fatorial (car lista))))
          (if (> fatorial-atual maior)
              (iterar (cdr lista) fatorial-atual)
              (iterar (cdr lista) maior)))))
  (iterar (cdr lista) (fatorial (car lista))))

;; Função principal
(define (main)
  (display "Digite uma lista de números separados por espaço: ")
  (flush-output)
  (let ((input (read-line)))
    (let ((numeros (map string->number (string-split input))))
      (display "Soma dos fatoriais: ")
      (display (soma-fatoriais numeros))
      (newline)
      (display "Maior fatorial: ")
      (display (maior-fatorial numeros))
      (newline))))

(main)
```

Este código em Scheme consiste em três funções principais:

1. A função `fatorial` calcula o fatorial de um número usando recursão. Ela retorna 1 quando o número é igual a 0, caso contrário, multiplica o número pelo fatorial do número anterior.
2. A função `soma-fatoriais` recebe uma lista de números e calcula a soma dos fatoriais de todos os números da lista. Ela utiliza a função `fatorial` para calcular o fatorial de cada número da lista e soma os resultados.
3. A função `maior-fatorial` recebe uma lista de números e encontra o maior fatorial presente na lista. Ela utiliza a função `fatorial` para calcular o fatorial de cada número e compara os resultados para determinar o maior.
4. A função `main` é a função principal do programa. Ela solicita ao usuário que digite uma lista de números separados por espaço, lê a entrada do usuário, converte-a em uma lista de números e, em seguida, chama as funções `soma-fatoriais` e `maior-fatorial` para exibir os resultados na saída.

Espero que este código atenda às suas necessidades!