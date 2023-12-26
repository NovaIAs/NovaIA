Claro! Aqui está um código complexo em Portugol:

```
algoritmo Complexo
var
   vetor: vetor[1..100] de inteiro
   somaTotal, somaImpares, somaPares: inteiro
   i: inteiro

procedimento ordenarVetor(var v: vetor[1..100] de inteiro)
var
   i, j, aux: inteiro

inicio
   para i de 1 ate 100 faca
      para j de i+1 ate 100 faca
         se v[i] > v[j] entao
            aux <- v[i]
            v[i] <- v[j]
            v[j] <- aux
         fimse
      fimpara
   fimpara
fimprocedimento

funcao calcularFatorial(n: inteiro): inteiro
var
   fatorial: inteiro

inicio
   se n = 0 entao
      retorne 1
   senao
      fatorial <- 1
      para i de 1 ate n faca
         fatorial <- fatorial * i
      fimpara
      retorne fatorial
   fimse
fimfuncao

procedimento imprimirResultado(somaTotal, somaImpares, somaPares: inteiro)
inicio
   escreva("A soma total dos elementos do vetor é: ", somaTotal)
   escreva("A soma dos números ímpares do vetor é: ", somaImpares)
   escreva("A soma dos números pares do vetor é: ", somaPares)
fimprocedimento

inicio
   somaTotal <- 0
   somaImpares <- 0
   somaPares <- 0

   // Preenchendo o vetor com valores aleatórios entre 1 e 100
   para i de 1 ate 100 faca
      vetor[i] <- aleatorio(1, 100)
      somaTotal <- somaTotal + vetor[i]
   fimpara

   // Ordenando o vetor em ordem crescente
   ordenarVetor(vetor)

   // Calculando a soma dos números ímpares e pares
   para i de 1 ate 100 faca
      se vetor[i] % 2 = 0 entao
         somaPares <- somaPares + vetor[i]
      senao
         somaImpares <- somaImpares + vetor[i]
      fimse
   fimpara

   // Imprimindo os resultados
   imprimirResultado(somaTotal, somaImpares, somaPares)

   // Calculando e imprimindo o fatorial de 10
   escreva("O fatorial de 10 é: ", calcularFatorial(10))
fimalgoritmo
```

Neste código complexo em Portugol, são realizadas as seguintes etapas:

1. Declaração de variáveis: Um vetor de tamanho 100 chamado "vetor" e três variáveis inteiras chamadas "somaTotal", "somaImpares" e "somaPares". Também é declarada a variável "i" para uso nos loops.
2. Procedimento "ordenarVetor": Este procedimento recebe o vetor como parâmetro e utiliza o algoritmo de ordenação Bubble Sort para ordenar os elementos do vetor em ordem crescente.
3. Função "calcularFatorial": Esta função recebe um número inteiro "n" como parâmetro e calcula o fatorial desse número.
4. Procedimento "imprimirResultado": Este procedimento recebe as três somas como parâmetros e imprime os resultados na tela.
5. No início do programa principal, as variáveis "somaTotal", "somaImpares" e "somaPares" são inicializadas com zero.
6. É utilizado um loop para preencher o vetor com valores aleatórios entre 1 e 100, ao mesmo tempo em que o valor de cada elemento é somado à variável "somaTotal".
7. O vetor é ordenado utilizando o procedimento "ordenarVetor".
8. Outro loop é utilizado para percorrer o vetor e calcular as somas dos números ímpares e pares.
9. Os resultados são impressos na tela utilizando o procedimento "imprimirResultado".
10. Por fim, é calculado o fatorial de 10 utilizando a função "calcularFatorial" e o resultado é impresso na tela.

Esse código realiza diversas operações complexas, como preenchimento de vetor, ordenação, cálculo de fatorial e manipulação de variáveis.