Claro! Aqui está um código em Portugol complexo e diferenciado:

```
algoritmo complexo

funcao principal()
    var
        matriz: vetor[1..100] de vetor[1..100] de inteiro
        somaLinhas: vetor[1..100] de inteiro
        somaColunas: vetor[1..100] de inteiro
        somaDiagonalPrincipal: inteiro
        somaDiagonalSecundaria: inteiro
    
    // Preenchendo a matriz com valores aleatórios entre 1 e 100
    para i de 1 ate 100 faca
        para j de 1 ate 100 faca
            matriz[i][j] <- aleatorio(1, 100)
        fimpara
    fimpara

    // Calculando a soma das linhas da matriz
    para i de 1 ate 100 faca
        soma <- 0
        para j de 1 ate 100 faca
            soma <- soma + matriz[i][j]
        fimpara
        somaLinhas[i] <- soma
    fimpara

    // Calculando a soma das colunas da matriz
    para j de 1 ate 100 faca
        soma <- 0
        para i de 1 ate 100 faca
            soma <- soma + matriz[i][j]
        fimpara
        somaColunas[j] <- soma
    fimpara

    // Calculando a soma da diagonal principal da matriz
    somaDiagonalPrincipal <- 0
    para i de 1 ate 100 faca
        somaDiagonalPrincipal <- somaDiagonalPrincipal + matriz[i][i]
    fimpara

    // Calculando a soma da diagonal secundária da matriz
    somaDiagonalSecundaria <- 0
    para i de 1 ate 100 faca
        somaDiagonalSecundaria <- somaDiagonalSecundaria + matriz[i][101 - i]
    fimpara

    // Exibindo os resultados
    escreva("Soma das linhas:")
    para i de 1 ate 100 faca
        escreva(somaLinhas[i], " ")
    fimpara

    escreva("\nSoma das colunas:")
    para j de 1 ate 100 faca
        escreva(somaColunas[j], " ")
    fimpara

    escreva("\nSoma da diagonal principal: ", somaDiagonalPrincipal)
    escreva("\nSoma da diagonal secundaria: ", somaDiagonalSecundaria)

fimfuncao
```

Neste código, criamos uma matriz de 100x100 preenchida com valores aleatórios entre 1 e 100. Em seguida, calculamos a soma das linhas, colunas, diagonal principal e diagonal secundária da matriz.

Para calcular a soma das linhas, percorremos cada linha da matriz e somamos todos os elementos. O resultado é armazenado no vetor `somaLinhas`.

Para calcular a soma das colunas, percorremos cada coluna da matriz e somamos todos os elementos. O resultado é armazenado no vetor `somaColunas`.

A soma da diagonal principal é calculada percorrendo a matriz diagonalmente de cima para baixo, somando os elementos onde a linha é igual à coluna.

A soma da diagonal secundária é calculada percorrendo a matriz diagonalmente de cima para baixo, somando os elementos onde a linha é igual a 101 - coluna.

Por fim, exibimos os resultados das somas das linhas, colunas, diagonal principal e diagonal secundária.