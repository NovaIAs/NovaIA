Claro! Vou criar um código complexo em C# para você. Este código gera uma matriz de números aleatórios e, em seguida, realiza a multiplicação de matrizes. 

```csharp
using System;

namespace MultiplicacaoMatrizes
{
    class Program
    {
        static void Main(string[] args)
        {
            int linhasA, colunasA, linhasB, colunasB;

            Console.WriteLine("Digite o número de linhas da matriz A:");
            linhasA = int.Parse(Console.ReadLine());

            Console.WriteLine("Digite o número de colunas da matriz A:");
            colunasA = int.Parse(Console.ReadLine());

            Console.WriteLine("Digite o número de linhas da matriz B:");
            linhasB = int.Parse(Console.ReadLine());

            Console.WriteLine("Digite o número de colunas da matriz B:");
            colunasB = int.Parse(Console.ReadLine());

            // Verifica se é possível realizar a multiplicação de matrizes
            if (colunasA != linhasB)
            {
                Console.WriteLine("Não é possível realizar a multiplicação de matrizes com as dimensões informadas.");
                return;
            }

            // Criação das matrizes
            int[,] matrizA = new int[linhasA, colunasA];
            int[,] matrizB = new int[linhasB, colunasB];
            int[,] matrizResultado = new int[linhasA, colunasB];

            // Preenchimento da matriz A com números aleatórios
            Random random = new Random();
            for (int i = 0; i < linhasA; i++)
            {
                for (int j = 0; j < colunasA; j++)
                {
                    matrizA[i, j] = random.Next(1, 10);
                }
            }

            // Preenchimento da matriz B com números aleatórios
            for (int i = 0; i < linhasB; i++)
            {
                for (int j = 0; j < colunasB; j++)
                {
                    matrizB[i, j] = random.Next(1, 10);
                }
            }

            // Multiplicação das matrizes
            for (int i = 0; i < linhasA; i++)
            {
                for (int j = 0; j < colunasB; j++)
                {
                    for (int k = 0; k < colunasA; k++)
                    {
                        matrizResultado[i, j] += matrizA[i, k] * matrizB[k, j];
                    }
                }
            }

            // Exibição da matriz resultado
            Console.WriteLine("\nMatriz Resultado:");
            for (int i = 0; i < linhasA; i++)
            {
                for (int j = 0; j < colunasB; j++)
                {
                    Console.Write(matrizResultado[i, j] + "\t");
                }
                Console.WriteLine();
            }
        }
    }
}
```

Este código começa solicitando ao usuário o número de linhas e colunas para as matrizes A e B. Em seguida, verifica se é possível realizar a multiplicação de matrizes com as dimensões informadas.

Depois disso, são criadas as matrizes A, B e a matriz resultado, todas com tamanho definido pelos valores inseridos pelo usuário.

A matriz A é preenchida com números aleatórios usando a classe `Random`. A matriz B também é preenchida da mesma forma.

Em seguida, ocorre a multiplicação das matrizes utilizando três loops aninhados. O primeiro loop percorre as linhas da matriz A, o segundo loop percorre as colunas da matriz B, e o terceiro loop percorre as colunas da matriz A e as linhas da matriz B para calcular o valor de cada elemento da matriz resultado.

Por fim, a matriz resultado é exibida na tela.