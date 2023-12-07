Claro! Aqui está um código complexo em Portugol:

```
algoritmo Complexo

funcao calcularFatorial(valor: inteiro): inteiro
    var resultado, i: inteiro
    resultado <- 1

    para i de 1 ate valor faca
        resultado <- resultado * i
    fimpara

    retorne resultado

fimfuncao

funcao calcularSerie(valor: real): real
    var i: inteiro
    var resultado, numerador, denominador: real

    resultado <- 0

    para i de 1 ate 10 faca
        numerador <- calcularFatorial(i)
        denominador <- (2 * i - 1)
        resultado <- resultado + (numerador / denominador)
    fimpara

    retorne resultado
fimfuncao

funcao calcularMedia(valores: vetor[5] de real): real
    var i: inteiro
    var soma: real

    soma <- 0

    para i de 0 ate 4 faca
        soma <- soma + valores[i]
    fimpara

    retorne soma / 5
fimfuncao

procedimento intercalarVetores(vetor1: vetor[5] de inteiro, vetor2: vetor[5] de inteiro): vetor[10] de inteiro
    var i, j, k: inteiro
    var vetorResultado: vetor[10] de inteiro

    i <- 0
    j <- 0
    k <- 0

    enquanto i < 5 e j < 5 faca
        se vetor1[i] < vetor2[j] entao
            vetorResultado[k] <- vetor1[i]
            i <- i + 1
        senao
            vetorResultado[k] <- vetor2[j]
            j <- j + 1
        fimse
        k <- k + 1
    fimenquanto

    enquanto i < 5 faca
        vetorResultado[k] <- vetor1[i]
        i <- i + 1
        k <- k + 1
    fimenquanto

    enquanto j < 5 faca
        vetorResultado[k] <- vetor2[j]
        j <- j + 1
        k <- k + 1
    fimenquanto

    retorne vetorResultado
fimprocedimento

inicio
    // Exemplo 1: Cálculo da série harmônica
    escreva("Informe um valor para a série harmônica: ")
    leia(valor)
    resultadoSerie <- calcularSerie(valor)
    escreva("O resultado da série harmônica é: ", resultadoSerie)

    // Exemplo 2: Cálculo da média de um vetor de valores
    var valores: vetor[5] de real
    var i: inteiro
    para i de 0 ate 4 faca
        escreva("Digite um valor: ")
        leia(valores[i])
    fimpara
    resultadoMedia <- calcularMedia(valores)
    escreva("A média dos valores informados é: ", resultadoMedia)

    // Exemplo 3: Intercalação de dois vetores ordenados
    var vetor1: vetor[5] de inteiro
    var vetor2: vetor[5] de inteiro
    var vetorResultado: vetor[10] de inteiro

    para i de 0 ate 4 faca
        escreva("Digite um valor para o vetor 1: ")
        leia(vetor1[i])
    fimpara

    para i de 0 ate 4 faca
        escreva("Digite um valor para o vetor 2: ")
        leia(vetor2[i])
    fimpara

    vetorResultado <- intercalarVetores(vetor1, vetor2)

    escreva("Vetor intercalado: ")
    para i de 0 ate 9 faca
        escreva(vetorResultado[i], " ")
    fimpara

fimalgoritmo
```

Este código complexo em Portugol apresenta três exemplos diferentes:

1. Cálculo da série harmônica: o usuário informa um valor e o código calcula a soma dos primeiros 10 termos da série harmônica utilizando a função calcularSerie.

2. Cálculo da média de um vetor de valores: o usuário digita 5 valores que são armazenados em um vetor e o código calcula a média utilizando a função calcularMedia.

3. Intercalação de dois vetores ordenados: o usuário digita 5 valores para cada um dos dois vetores, que são ordenados em ordem crescente, utilizando o procedimento intercalarVetores. O vetor resultante é impresso no final do programa.

Espero que este código complexo atenda às suas expectativas!