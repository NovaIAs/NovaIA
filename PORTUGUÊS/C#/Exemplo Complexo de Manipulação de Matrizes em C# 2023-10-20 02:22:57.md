Claro! Aqui está um exemplo de código complexo em C#:

```csharp
using System;

namespace ExemploComplexo
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bem-vindo ao exemplo complexo!");

            int tamanho = 10;
            int[,] matriz = new int[tamanho, tamanho];

            // Preenche a matriz com valores aleatórios
            Random random = new Random();
            for (int i = 0; i < tamanho; i++)
            {
                for (int j = 0; j < tamanho; j++)
                {
                    matriz[i, j] = random.Next(1, 100);
                }
            }

            Console.WriteLine("Matriz original:");
            ImprimirMatriz(matriz, tamanho);

            int[,] matrizTransposta = TransporMatriz(matriz, tamanho);

            Console.WriteLine("Matriz transposta:");
            ImprimirMatriz(matrizTransposta, tamanho);

            Console.WriteLine("Pressione qualquer tecla para sair...");
            Console.ReadKey();
        }

        static int[,] TransporMatriz(int[,] matriz, int tamanho)
        {
            int[,] matrizTransposta = new int[tamanho, tamanho];

            for (int i = 0; i < tamanho; i++)
            {
                for (int j = 0; j < tamanho; j++)
                {
                    matrizTransposta[i, j] = matriz[j, i];
                }
            }

            return matrizTransposta;
        }

        static void ImprimirMatriz(int[,] matriz, int tamanho)
        {
            for (int i = 0; i < tamanho; i++)
            {
                for (int j = 0; j < tamanho; j++)
                {
                    Console.Write(matriz[i, j] + " ");
                }
                Console.WriteLine();
            }
            Console.WriteLine();
        }
    }
}
```

Este código implementa um exemplo complexo em C# que envolve a criação e manipulação de matrizes.

Primeiro, o programa define a classe `Program` e o método `Main`, que é o ponto de entrada do programa. Dentro do método `Main`, é exibida uma mensagem de boas-vindas.

Em seguida, o código cria uma matriz bidimensional chamada `matriz` com um tamanho definido (neste exemplo, 10x10). Essa matriz é preenchida com valores aleatórios usando a classe `Random`.

Depois disso, a matriz original é exibida na tela usando o método `ImprimirMatriz`. Em seguida, a matriz é transposta (ou seja, as linhas se tornam colunas e vice-versa) usando o método `TransporMatriz`.

A matriz transposta é então exibida na tela novamente usando o método `ImprimirMatriz`.

Por fim, o programa aguarda que o usuário pressione qualquer tecla antes de encerrar.

Espero que este exemplo complexo em C# seja útil para você!