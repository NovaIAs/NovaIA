Claro! Aqui está um código complexo escrito na linguagem D:

```d
import std.stdio;
import std.algorithm;

void main()
{
    // Definindo uma matriz de números aleatórios
    int[][] matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

    // Calculando a soma de cada linha da matriz
    int[] somaLinhas;
    foreach (linha; matriz)
    {
        int soma = 0;
        foreach (elemento; linha)
        {
            soma += elemento;
        }
        somaLinhas ~= soma;
    }

    // Calculando a soma de cada coluna da matriz
    int[] somaColunas;
    for (size_t coluna = 0; coluna < matriz[0].length; coluna++)
    {
        int soma = 0;
        foreach (linha; matriz)
        {
            soma += linha[coluna];
        }
        somaColunas ~= soma;
    }

    // Encontrando a maior soma de linha e coluna
    int maiorSomaLinha = maxElement(somaLinhas);
    int maiorSomaColuna = maxElement(somaColunas);

    // Imprimindo a matriz original
    writeln("Matriz original:");
    foreach (linha; matriz)
    {
        writeln(linha);
    }

    // Imprimindo a soma de cada linha
    writeln("Soma de cada linha:");
    foreach (soma; somaLinhas)
    {
        writeln(soma);
    }

    // Imprimindo a soma de cada coluna
    writeln("Soma de cada coluna:");
    foreach (soma; somaColunas)
    {
        writeln(soma);
    }

    // Imprimindo a maior soma de linha e coluna
    writeln("Maior soma de linha:", maiorSomaLinha);
    writeln("Maior soma de coluna:", maiorSomaColuna);
}
```

Este código cria uma matriz de números aleatórios e calcula a soma de cada linha e coluna da matriz. Em seguida, encontra a maior soma de linha e coluna e imprime os resultados.

Primeiro, a matriz é definida como um array bidimensional de inteiros. Em seguida, são utilizados loops `foreach` para percorrer cada linha da matriz e calcular a soma de seus elementos. Os resultados das somas de linha são armazenados em um array chamado `somaLinhas`.

Em seguida, é utilizado um loop `for` para percorrer cada coluna da matriz e calcular a soma de seus elementos. Os resultados das somas de coluna são armazenados em um array chamado `somaColunas`.

Depois disso, as maiores somas de linha e coluna são encontradas usando a função `maxElement` da biblioteca `std.algorithm`.

Por fim, o código imprime a matriz original, seguida das somas de cada linha e coluna. Além disso, também imprime a maior soma de linha e coluna.

Espero que isso atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, não hesite em perguntar.