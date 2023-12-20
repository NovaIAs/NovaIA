Claro! Aqui está um exemplo de código complexo em C#:

```csharp
using System;

namespace ExemploComplexo
{
    class Program
    {
        static void Main(string[] args)
        {
            // Declaração e inicialização das variáveis
            int tamanho = 10;
            int[] numeros = new int[tamanho];
            int soma = 0;

            // Preenchendo o array com números aleatórios entre 1 e 100
            Random random = new Random();
            for (int i = 0; i < tamanho; i++)
            {
                numeros[i] = random.Next(1, 101);
            }

            // Imprimindo o array de números
            Console.WriteLine("Array de números gerados aleatoriamente:");
            for (int i = 0; i < tamanho; i++)
            {
                Console.Write(numeros[i] + " ");
            }
            Console.WriteLine();

            // Calculando a soma dos números pares e ímpares
            for (int i = 0; i < tamanho; i++)
            {
                if (numeros[i] % 2 == 0)
                {
                    soma += numeros[i]; // soma dos números pares
                }
                else
                {
                    soma -= numeros[i]; // soma dos números ímpares
                }
            }

            // Imprimindo a soma dos números pares e ímpares
            Console.WriteLine("Soma dos números pares: " + soma);
            Console.WriteLine("Soma dos números ímpares: " + (soma * -1));

            // Ordenando o array em ordem crescente usando Bubble Sort
            for (int i = 0; i < tamanho - 1; i++)
            {
                for (int j = 0; j < tamanho - i - 1; j++)
                {
                    if (numeros[j] > numeros[j + 1])
                    {
                        int temp = numeros[j];
                        numeros[j] = numeros[j + 1];
                        numeros[j + 1] = temp;
                    }
                }
            }

            // Imprimindo o array de números ordenado
            Console.WriteLine("Array de números ordenado em ordem crescente:");
            for (int i = 0; i < tamanho; i++)
            {
                Console.Write(numeros[i] + " ");
            }
            Console.WriteLine();

            // Verificando se o array possui números repetidos
            bool temRepeticao = false;
            for (int i = 0; i < tamanho; i++)
            {
                for (int j = i + 1; j < tamanho; j++)
                {
                    if (numeros[i] == numeros[j])
                    {
                        temRepeticao = true;
                        break;
                    }
                }
                if (temRepeticao)
                {
                    break;
                }
            }

            // Imprimindo se o array possui números repetidos ou não
            if (temRepeticao)
            {
                Console.WriteLine("O array possui números repetidos.");
            }
            else
            {
                Console.WriteLine("O array não possui números repetidos.");
            }
        }
    }
}
```

Este código contém várias funcionalidades complexas. Primeiro, ele gera um array de números aleatórios entre 1 e 100. Em seguida, calcula a soma dos números pares e ímpares presentes no array. Depois, ordena o array em ordem crescente usando o algoritmo Bubble Sort. Por fim, verifica se o array possui números repetidos. Todos os resultados são impressos no console.

Espero que este exemplo atenda às suas expectativas!