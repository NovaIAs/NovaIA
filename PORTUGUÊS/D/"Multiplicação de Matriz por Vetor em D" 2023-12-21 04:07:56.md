Claro! Aqui está um exemplo de código complexo em D que gera uma matriz de números aleatórios e, em seguida, realiza a multiplicação dessa matriz por um vetor:

```D
import std.random;
import std.stdio;
import std.array;

void main()
{
    int linhas = 3;
    int colunas = 3;
    
    int[][] matriz = new int[][linhas];
    
    foreach (i; 0..linhas)
    {
        matriz[i] = new int[colunas];
        
        foreach (j; 0..colunas)
        {
            matriz[i][j] = uniform(1, 10); // Gera um número aleatório entre 1 e 10
        }
    }
    
    int[] vetor = new int[colunas];
    
    foreach (i; 0..colunas)
    {
        vetor[i] = uniform(1, 10);
    }
    
    int[] vetorResultado = multiplicarMatrizVetor(matriz, vetor);
    
    writeln("Matriz:");
    imprimirMatriz(matriz);
    
    writeln();
    
    writeln("Vetor:");
    imprimirVetor(vetor);
    
    writeln();
    
    writeln("Vetor Resultado:");
    imprimirVetor(vetorResultado);
}

int[] multiplicarMatrizVetor(int[][] matriz, int[] vetor)
{
    int[] vetorResultado = new int[matriz.length];
    
    foreach (i; 0..matriz.length)
    {
        int soma = 0;
        
        foreach (j; 0..matriz[i].length)
        {
            soma += matriz[i][j] * vetor[j];
        }
        
        vetorResultado[i] = soma;
    }
    
    return vetorResultado;
}

void imprimirMatriz(int[][] matriz)
{
    foreach (linha; matriz)
    {
        foreach (elemento; linha)
        {
            writef("%3d ", elemento);
        }
        
        writeln();
    }
}

void imprimirVetor(int[] vetor)
{
    foreach (elemento; vetor)
    {
        writef("%3d ", elemento);
    }
    
    writeln();
}
```

Neste código, primeiro definimos o número de linhas e colunas da matriz. Em seguida, criamos a matriz e preenchemos cada elemento com um número aleatório entre 1 e 10. Também criamos um vetor e preenchemos cada elemento com um número aleatório entre 1 e 10.

Depois disso, chamamos a função `multiplicarMatrizVetor`, que recebe a matriz e o vetor como parâmetros e retorna um vetor resultado da multiplicação da matriz pelo vetor. Essa função realiza a multiplicação de cada linha da matriz pelo vetor correspondente, somando os produtos.

Por fim, chamamos as funções `imprimirMatriz` e `imprimirVetor` para exibir a matriz original, o vetor original e o vetor resultado na saída.

Espero que isso atenda às suas expectativas!