Claro! Aqui está um código complexo em Portugol que realiza o cálculo de uma sequência de Fibonacci até um determinado número. O código utiliza recursão para calcular os valores.

```
algoritmo fibonacci
var
   n, i: inteiro

funcao fib(n)
inicio
   se n <= 1 entao
      retorne n
   senao
      retorne fib(n - 1) + fib(n - 2)
   fimse
fimfuncao

inicio
   escreva("Digite o número limite para a sequência de Fibonacci: ")
   leia(n)

   escreva("Sequência de Fibonacci até ", n, " :")

   para i de 0 ate n faca
      escreva(fib(i), " ")
   fimpara
fimalgoritmo
```

Explicação do código:

1. O programa começa com a declaração das variáveis `n` e `i`.

2. Em seguida, é definida uma função `fib(n)` que calcula o valor da sequência de Fibonacci para um dado `n`. A função utiliza recursão para realizar os cálculos.

3. No bloco principal do programa, o usuário é solicitado a digitar o número limite para a sequência de Fibonacci.

4. A seguinte mensagem é exibida: "Sequência de Fibonacci até `n` :"

5. Utilizando um loop `para`, o programa itera de `0` até `n` e chama a função `fib(i)` para calcular cada valor da sequência. Os valores são exibidos na tela.

Dessa forma, o código permite calcular e exibir a sequência de Fibonacci até um número determinado pelo usuário, utilizando recursão.