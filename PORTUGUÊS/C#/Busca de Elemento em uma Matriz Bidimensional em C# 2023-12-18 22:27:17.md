Claro! Aqui está um exemplo de um código complexo em C# que realiza a busca de um elemento em uma matriz bidimensional:

```csharp
using System;

class Program
{
    static void Main()
    {
        int[,] matriz = new int[5, 5] 
        {
            { 1, 2, 3, 4, 5 },
            { 6, 7, 8, 9, 10 },
            { 11, 12, 13, 14, 15 },
            { 16, 17, 18, 19, 20 },
            { 21, 22, 23, 24, 25 }
        };

        Console.WriteLine("Digite o valor a ser buscado na matriz: ");
        int valor = Convert.ToInt32(Console.ReadLine());

        bool encontrado = BuscarElemento(matriz, valor);

        if (encontrado)
        {
            Console.WriteLine("Elemento encontrado!");
        }
        else
        {
            Console.WriteLine("Elemento não encontrado!");
        }
    }

    static bool BuscarElemento(int[,] matriz, int valor)
    {
        int linhas = matriz.GetLength(0);
        int colunas = matriz.GetLength(1);

        for (int i = 0; i < linhas; i++)
        {
            for (int j = 0; j < colunas; j++)
            {
                if (matriz[i, j] == valor)
                {
                    return true;
                }
            }
        }

        return false;
    }
}
```

Este código começa declarando uma matriz bidimensional `matriz` com dimensões de 5x5 e preenchendo-a com valores de 1 a 25. Em seguida, o usuário é solicitado a digitar um valor a ser buscado na matriz.

A função `BuscarElemento` é responsável por percorrer a matriz e verificar se o valor fornecido está presente. Ela utiliza um loop aninhado para percorrer todas as células da matriz e compara o valor de cada célula com o valor de busca. Se o valor for encontrado, a função retorna `true`. Caso contrário, ela retorna `false`.

No método `Main`, o usuário é solicitado a digitar o valor a ser buscado. Em seguida, o método `BuscarElemento` é chamado e o resultado é exibido na tela.

Espero que este código atenda às suas expectativas!