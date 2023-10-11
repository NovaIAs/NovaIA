Claro! Aqui está um código complexo em D que envolve a criação de uma matriz e a aplicação de um algoritmo de ordenação chamado Bubble Sort:

```d
import std.stdio;
import std.algorithm.iteration;
import std.array;

void main()
{
    writeln("Digite o número de linhas e colunas da matriz: ");
    
    int rows, cols;
    readf("%d %d", &rows, &cols);
    
    // Criando a matriz
    int[][] matrix;
    matrix.length = rows;
    
    foreach (ref row; matrix)
    {
        row.length = cols;
    }
    
    // Preenchendo a matriz com valores
    writeln("Digite os elementos da matriz: ");
    
    foreach (ref row; matrix)
    {
        foreach (ref item; row)
        {
            readf("%d", &item);
        }
    }
    
    // Exibindo a matriz original
    writeln("Matriz original:");
    foreach (row; matrix)
    {
        writeln(row);
    }
    
    // Aplicando o algoritmo Bubble Sort nas linhas da matriz
    foreach (ref row; matrix)
    {
        row.sort();
    }
    
    // Exibindo a matriz ordenada
    writeln("Matriz ordenada:");
    foreach (row; matrix)
    {
        writeln(row);
    }
}
```

Este código em D permite ao usuário informar o número de linhas e colunas de uma matriz e, em seguida, preenche essa matriz com valores digitados pelo usuário. Em seguida, aplica o algoritmo de ordenação Bubble Sort em cada linha da matriz e exibe a matriz original e a matriz ordenada.

O código começa importando os módulos std.stdio, std.algorithm.iteration e std.array para permitir a leitura dos dados de entrada, a iteração sobre a matriz e a manipulação de arrays, respectivamente.

Em seguida, a função `main()` é definida como ponto de entrada do programa. Dentro da função `main()`, o usuário é solicitado a informar o número de linhas e colunas da matriz usando a função `readf()`. Em seguida, a matriz é criada usando o operador `length` para definir a quantidade de linhas e a quantidade de colunas em cada linha.

Depois disso, o usuário é solicitado a digitar os elementos da matriz usando um laço `foreach`. Cada elemento é lido usando a função `readf()` e armazenado na matriz.

A matriz original é exibida usando outro laço `foreach` e, em seguida, o algoritmo Bubble Sort é aplicado a cada linha da matriz usando outro laço `foreach`. O Bubble Sort é um algoritmo de ordenação simples que compara pares adjacentes de elementos e os troca de posição se estiverem fora de ordem. Esse processo é repetido até que a matriz esteja completamente ordenada.

Por fim, a matriz ordenada é exibida usando mais um laço `foreach`.

Esse código complexo em D demonstra o uso de estruturas de controle, manipulação de arrays multidimensionais, entrada e saída de dados e aplicação de um algoritmo de ordenação em uma matriz.