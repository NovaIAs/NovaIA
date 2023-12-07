Certamente! Aqui está um código complexo em D:

```
import std.stdio;

void main()
{
    writeln("Bem-vindo ao Código Complexo em D!");

    // Criando uma matriz
    int[][] matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

    // Imprimindo a matriz inicial
    writeln("Matriz inicial:");
    imprimirMatriz(matriz);

    // Transpondo a matriz
    int[][] matrizTransposta = transporMatriz(matriz);

    // Imprimindo a matriz transposta
    writeln("Matriz transposta:");
    imprimirMatriz(matrizTransposta);

    // Verificando se a matriz é simétrica
    bool simetrica = verificarSimetria(matriz, matrizTransposta);

    // Imprimindo o resultado da verificação
    if (simetrica)
        writeln("A matriz é simétrica!");
    else
        writeln("A matriz não é simétrica!");

    writeln("Fim do programa.");
}

// Função para imprimir uma matriz
void imprimirMatriz(T)(T[][] matriz)
{
    foreach (linha; matriz)
    {
        foreach (elemento; linha)
            writef("%3d ", elemento);
        
        writeln();
    }
}

// Função para transpor uma matriz
int[][] transporMatriz(int[][] matriz)
{
    int[][] matrizTransposta;

    for (size_t i = 0; i < matriz[0].length; i++)
    {
        int[] coluna;
        for (size_t j = 0; j < matriz.length; j++)
            coluna ~= matriz[j][i];
        
        matrizTransposta ~= coluna;
    }

    return matrizTransposta;
}

// Função para verificar se uma matriz é simétrica
bool verificarSimetria(int[][] matriz1, int[][] matriz2)
{
    if (matriz1.length != matriz2.length || matriz1[0].length != matriz2[0].length)
        return false;
    
    for (size_t i = 0; i < matriz1.length; i++)
    {
        for (size_t j = 0; j < matriz1[0].length; j++)
        {
            if (matriz1[i][j] != matriz2[i][j])
                return false;
        }
    }

    return true;
}
```

Neste código, estamos trabalhando com matrizes em D. Primeiro, criamos uma matriz de 3x3. Em seguida, transpomos essa matriz para obter sua matriz transposta. Depois, verificamos se a matriz original é simétrica, comparando-a com sua matriz transposta. Por fim, imprimimos os resultados na tela. A função `imprimirMatriz` é responsável por exibir uma matriz legível para o usuário, enquanto as funções `transporMatriz` e `verificarSimetria` executam os cálculos necessários.

Espero que isso atenda aos seus requisitos! Se você tiver alguma outra solicitação, não hesite em me perguntar. Estou aqui para ajudar!