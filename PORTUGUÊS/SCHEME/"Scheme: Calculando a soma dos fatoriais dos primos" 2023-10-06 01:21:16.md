Claro! Aqui está um exemplo de um código complexo escrito em Scheme:

```
;; Definindo uma função recursiva para calcular o fatorial de um número
(define (fatorial n)
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

;; Definindo uma função para verificar se um número é primo
(define (primo? n)
  (define (iter i)
    (cond ((= i 1) #t)
          ((= (modulo n i) 0) #f)
          (else (iter (- i 1)))))
  (iter (floor (sqrt n))))

;; Definindo uma função para calcular a soma dos fatoriais dos números primos até um determinado limite
(define (soma_fatoriais_primos limite)
  (define (iter n soma)
    (if (> n limite)
        soma
        (if (primo? n)
            (iter (+ n 1) (+ soma (fatorial n)))
            (iter (+ n 1) soma))))
  (iter 2 0))

;; Exemplo de uso da função soma_fatoriais_primos
(display "Digite um limite: ")
(let ((limite (read)))
  (display "A soma dos fatoriais dos números primos até o limite é: ")
  (display (soma_fatoriais_primos limite)))
```

Neste código, temos três funções principais:

1. A função `fatorial` recebe um número inteiro `n` e retorna o fatorial desse número. Ela é definida de forma recursiva, onde o fatorial de `n` é igual a `n` multiplicado pelo fatorial de `n-1`.

2. A função `primo?` recebe um número inteiro `n` e verifica se ele é primo. Ela utiliza uma função auxiliar `iter` que itera de `i = 1` até `i = n/2`, verificando se o resto da divisão de `n` por `i` é igual a zero. Se encontrar um divisor, retorna `false`, caso contrário, retorna `true`.

3. A função `soma_fatoriais_primos` recebe um limite inteiro e calcula a soma dos fatoriais de todos os números primos até esse limite. Ela utiliza uma função auxiliar `iter` que itera de `n = 2` até `n = limite`, verificando se `n` é primo. Se for, soma o fatorial de `n` à variável `soma` e continua a iteração. Caso contrário, apenas continua a iteração. No final, retorna `soma`.

O código também possui uma parte interativa, onde o usuário é solicitado a digitar um limite. Esse limite é lido e passado como argumento para a função `soma_fatoriais_primos`. O resultado é então exibido na tela.