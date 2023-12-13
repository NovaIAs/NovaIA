Claro! Aqui está um código complexo em Scheme, que utiliza a recursão para implementar a função de Fibonacci. A função de Fibonacci retorna o n-ésimo número da sequência de Fibonacci, onde cada número é a soma dos dois anteriores. O código está em português e contém comentários explicativos.

```scheme
;; Definição da função de Fibonacci
(define (fibonacci n)
  ;; Função auxiliar recursiva que calcula o n-ésimo número de Fibonacci
  (define (fibonacci-aux n a b)
    ;; Caso base: quando n = 0, retorna o valor atual de a
    (if (= n 0)
        a
        ;; Caso contrário, chama fibonacci-aux com n-1, b e a + b
        (fibonacci-aux (- n 1) b (+ a b))))
  
  ;; Chama a função auxiliar com os valores iniciais de a = 0 e b = 1
  (fibonacci-aux n 0 1))

;; Exemplo de uso da função de Fibonacci
(displayln "Digite um número inteiro:")
(define numero (read))
(displayln (string-append "O " (number->string numero) "º número de Fibonacci é: " (number->string (fibonacci numero))))
```

Neste código, a função `fibonacci` recebe um número inteiro `n` como parâmetro e retorna o n-ésimo número da sequência de Fibonacci. A função `fibonacci-aux` é uma função auxiliar recursiva que realiza o cálculo.

Dentro da função `fibonacci-aux`, temos um caso base, que é quando `n` é igual a zero. Nesse caso, o valor atual de `a` é retornado. Caso contrário, a função chama a si mesma com `n-1`, atualizando os valores de `b` para `a` e `a + b` para `b`. Esse processo continua até que `n` seja zero, retornando assim o n-ésimo número de Fibonacci.

No exemplo de uso da função, o código solicita ao usuário que digite um número inteiro, armazena esse valor na variável `numero` e, em seguida, exibe na tela o n-ésimo número de Fibonacci correspondente ao valor digitado.