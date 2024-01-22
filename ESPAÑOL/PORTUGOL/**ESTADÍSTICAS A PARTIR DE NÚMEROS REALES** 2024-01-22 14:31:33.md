```portuguol
程序 inicio
   inteiro contador, n, maior, menor, soma, media
  real fracao

  escreva "Digite um número inteiro positivo: "
  leia n

  maior := 0
  menor := 1000000
  soma := 0

  para contador de 1 até n faca
    escreva "Digite um número real: "
    leia fracao

    soma := soma + fracao

    se fracao > maior entao
      maior := fracao
    fim se

    se fracao < menor entao
      menor := fracao
    fim se
  fim para

  media := soma / n

  escreva "O maior número digitado foi: ", maior
  escreva "O menor número digitado foi: ", menor
  escreva "A soma dos números digitados foi: ", soma
  escreva "A média dos números digitados foi: ", media
fim do programa
```

Este código lê um número inteiro positivo `n` e, em seguida, lê `n` números reais. O código então calcula o maior, o menor, a soma e a média dos números lidos. Por fim, exibe os resultados na tela.

O código usa uma estrutura de repetição `para` para ler os `n` números reais. O código também usa variáveis para armazenar o maior, o menor, a soma e a média dos números lidos.

O código usa a função `escreva` para exibir os resultados na tela. A função `leia` é usada para ler o número inteiro `n` e os `n` números reais.

O código é complexo porque usa uma estrutura de repetição, variáveis e funções. O código também usa operadores aritméticos para calcular o maior, o menor, a soma e a média dos números lidos.