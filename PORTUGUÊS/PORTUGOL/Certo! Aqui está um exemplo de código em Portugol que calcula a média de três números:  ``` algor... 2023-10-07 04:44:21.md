Claro! Aqui está um exemplo de código complexo em Portugol:

```
algoritmo codigoComplexo

// Variáveis globais
var
  matriz: vetor[1..10, 1..10] de inteiro
  somaLinhas: vetor[1..10] de inteiro
  somaColunas: vetor[1..10] de inteiro

// Função para calcular a soma das linhas da matriz
funcao calcularSomaLinhas(matriz: vetor[1..10, 1..10] de inteiro): vetor[1..10] de inteiro
var
  soma: vetor[1..10] de inteiro
  i, j: inteiro

para i de 1 ate 10 faca
  soma[i] <- 0
  para j de 1 ate 10 faca
    soma[i] <- soma[i] + matriz[i, j]
  fimpara
fimpara

retorne soma

fimfuncao

// Função para calcular a soma das colunas da matriz
funcao calcularSomaColunas(matriz: vetor[1..10, 1..10] de inteiro): vetor[1..10] de inteiro
var
  soma: vetor[1..10] de inteiro
  i, j: inteiro

para j de 1 ate 10 faca
  soma[j] <- 0
  para i de 1 ate 10 faca
    soma[j] <- soma[j] + matriz[i, j]
  fimpara
fimpara

retorne soma

fimfuncao

inicio

// Preenchendo a matriz com valores aleatórios
para i de 1 ate 10 faca
  para j de 1 ate 10 faca
    matriz[i, j] <- aleatorio(1, 100)
  fimpara
fimpara

// Calculando a soma das linhas
somaLinhas <- calcularSomaLinhas(matriz)

// Calculando a soma das colunas
somaColunas <- calcularSomaColunas(matriz)

// Exibindo a matriz
para i de 1 ate 10 faca
  para j de 1 ate 10 faca
    escreva(matriz[i, j], " ")
  fimpara
  escreval("")
fimpara

// Exibindo a soma das linhas
escreval("Soma das linhas:")
para i de 1 ate 10 faca
  escreva(somaLinhas[i], " ")
fimpara
escreval("")

// Exibindo a soma das colunas
escreval("Soma das colunas:")
para j de 1 ate 10 faca
  escreva(somaColunas[j], " ")
fimpara
escreval("")

fimalgoritmo
```

Este código em Portugol cria uma matriz 10x10 preenchida com valores aleatórios entre 1 e 100. Em seguida, calcula a soma das linhas e das colunas da matriz. Por fim, exibe a matriz e as somas das linhas e colunas.

As funções `calcularSomaLinhas` e `calcularSomaColunas` recebem a matriz como parâmetro e retornam um vetor com as somas das linhas e colunas, respectivamente. Elas utilizam dois laços aninhados para percorrer a matriz e somar os elementos.

Espero que esse código atenda às suas expectativas! Caso precise de mais ajuda, estou à disposição.