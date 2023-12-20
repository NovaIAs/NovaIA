Claro! Aqui está um exemplo de um código complexo em Scheme:

```scheme
;; Função recursiva para calcular o fatorial de um número
(define (fatorial n)
  (if (= n 0) ; Se o número for igual a 0
      1       ; Retorna 1
      (* n (fatorial (- n 1))))) ; Caso contrário, multiplica o número pelo fatorial do número anterior

;; Função para calcular a sequência de Fibonacci até o número n
(define (fibonacci n)
  (if (<= n 0) ; Se o número for menor ou igual a 0
      '()       ; Retorna uma lista vazia
      (let loop ((n n) (a 0) (b 1) (result '())) ; Utiliza uma função interna loop para realizar a iteração
        (if (= n 1) ; Se o número for igual a 1
            (reverse (cons a result)) ; Retorna a lista resultante invertida
            (loop (- n 1) b (+ a b) (cons a result)))))) ; Caso contrário, realiza a iteração da sequência

;; Função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((> (* i i) n) #t) ; Se i*i for maior que o número, o número é primo
          ((zero? (modulo n i)) #f) ; Se o número for divisível por i, o número não é primo
          (else (iter (+ i 1))))) ; Caso contrário, chama a função iter com o próximo número
  (if (< n 2) #f (iter 2))) ; Se o número for menor que 2, não é primo. Caso contrário, chama a função iter com 2

;; Função principal para testar as funções acima
(define (main)
  (display "Digite um número: ")
  (flush-output)
  (let ((n (read))) ; Lê um número do teclado
    (display "O fatorial desse número é: ")
    (display (fatorial n)) ; Calcula e exibe o fatorial do número
    (newline)
    (display "A sequência de Fibonacci até esse número é: ")
    (display (fibonacci n)) ; Calcula e exibe a sequência de Fibonacci até o número
    (newline)
    (display "Esse número é primo? ")
    (display (primo? n)))) ; Verifica se o número é primo e exibe o resultado

;; Chama a função principal para iniciar o programa
(main)
```

Explicação do código:

1. A função `fatorial` é definida utilizando uma abordagem recursiva. Ela recebe um número `n` e verifica se ele é igual a 0. Se for, retorna 1. Caso contrário, multiplica o número `n` pelo fatorial do número anterior, chamando a função `fatorial` novamente com `n-1`.

2. A função `fibonacci` também utiliza recursão, mas de forma um pouco mais complexa. Ela recebe um número `n` como argumento e verifica se ele é menor ou igual a 0. Se for, retorna uma lista vazia. Caso contrário, utiliza uma função interna chamada `loop` para iterar e calcular a sequência de Fibonacci até o número `n`. A função `loop` recebe o número atual `n`, dois números auxiliares `a` e `b`, e uma lista `result` como argumentos. Ela verifica se `n` é igual a 1. Se for, retorna a lista `result` com o número `a` adicionado. Caso contrário, chama a função `loop` novamente com `n-1`, o próximo número da sequência (`b` é somado a `a`), e a lista `result` com o número `a` adicionado. Após a iteração, a função `fibonacci` retorna a lista `result` invertida.

3. A função `primo?` verifica se um número `n` é primo. Ela utiliza uma função interna chamada `iter` para realizar uma iteração de 2 até a raiz quadrada do número `n`. A função `iter` verifica se o número `n` é divisível por `i`. Se for, retorna falso. Caso contrário, chama a função `iter` com o próximo número `i+1`. Se a iteração for concluída sem encontrar um divisor, o número é primo e retorna verdadeiro.

4. A função `main` é a função principal do programa. Ela solicita ao usuário que digite um número, lê o número do teclado, e em seguida chama as funções `fatorial`, `fibonacci` e `primo?` para calcular e exibir o fatorial do número, a sequência de Fibonacci até o número, e se o número é primo ou não.

5. Por fim, a função principal `main` é chamada para iniciar o programa.