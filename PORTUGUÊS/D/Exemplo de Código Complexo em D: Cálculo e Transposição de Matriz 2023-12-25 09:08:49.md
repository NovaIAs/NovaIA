Claro! Aqui está um exemplo de um código complexo na linguagem D:

```d
import std.stdio;

void main()
{
    int[][] matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    int soma = 0;
    
    foreach (int[] linha; matriz)
    {
        foreach (int elemento; linha)
        {
            soma += elemento;
        }
    }
    
    writeln("A soma de todos os elementos da matriz é ", soma);
    
    int maiorElemento = encontrarMaiorElemento(matriz);
    writeln("O maior elemento da matriz é ", maiorElemento);
    
    int[][] matrizTransposta = obterMatrizTransposta(matriz);
    writeln("A matriz transposta é: ");
    imprimirMatriz(matrizTransposta);
}

int encontrarMaiorElemento(int[][] matriz)
{
    int maior = matriz[0][0];
    
    foreach (int[] linha; matriz)
    {
        foreach (int elemento; linha)
        {
            if (elemento > maior)
            {
                maior = elemento;
            }
        }
    }
    
    return maior;
}

int[][] obterMatrizTransposta(int[][] matriz)
{
    int[][] matrizTransposta;
    int numLinhas = matriz.length;
    int numColunas = matriz[0].length;
    
    for (int i = 0; i < numColunas; i++)
    {
        int[] novaLinha;
        
        for (int j = 0; j < numLinhas; j++)
        {
            novaLinha ~= matriz[j][i];
        }
        
        matrizTransposta ~= novaLinha;
    }
    
    return matrizTransposta;
}

void imprimirMatriz(int[][] matriz)
{
    foreach (int[] linha; matriz)
    {
        foreach (int elemento; linha)
        {
            write(elemento, "\t");
        }
        
        writeln();
    }
}
```

Este código cria uma matriz de números inteiros, calcula a soma de todos os elementos da matriz, encontra o maior elemento, obtém a matriz transposta e imprime a matriz na saída padrão. 

A função `encontrarMaiorElemento` percorre todos os elementos da matriz e retorna o maior encontrado. A função `obterMatrizTransposta` cria uma nova matriz, onde as linhas e colunas são invertidas em relação à matriz original. A função `imprimirMatriz` percorre a matriz e imprime cada elemento separado por uma tabulação.

Espero que este exemplo atenda às suas expectativas!