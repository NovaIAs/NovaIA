```portuguol
inicio
  variavel numero, contador, soma: inteiro
  soma <- 0
  contador <- 1
  para numero <- 1 ate 100 faca
    se numero mod 2 = 0 entao
      soma <- soma + numero
      contador <- contador + 1
    fimse
  fimpara
  escreva("A soma dos números pares entre 1 e 100 é: ", soma)
  escreva("O número de números pares entre 1 e 100 é: ", contador)
fim

```
Este código calcula a soma dos números pares entre 1 e 100. Ele também conta o número de números pares nesse intervalo. O código usa um laço `para` para iterar sobre cada número de 1 a 100. Para cada número, ele usa a operação de módulo (`mod`) para verificar se o número é par. Se o número for par, ele é adicionado à variável `soma` e o contador é incrementado. No final do laço, o código exibe a soma dos números pares e o número de números pares.