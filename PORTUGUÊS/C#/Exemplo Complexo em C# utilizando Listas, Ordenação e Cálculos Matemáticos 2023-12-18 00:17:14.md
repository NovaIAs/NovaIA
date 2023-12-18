Certamente! Aqui está um exemplo de um código complexo em C# que utiliza uma série de conceitos avançados de programação:

```csharp
using System;

namespace ExemploComplexo
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bem-vindo ao exemplo complexo!");

            // Criando uma lista de números
            List<int> numeros = new List<int>();

            // Preenchendo a lista com números aleatórios
            Random random = new Random();
            for (int i = 0; i < 10; i++)
            {
                int numeroAleatorio = random.Next(1, 100);
                numeros.Add(numeroAleatorio);
            }

            // Imprimindo a lista de números
            Console.WriteLine("Números gerados:");
            foreach (int numero in numeros)
            {
                Console.Write(numero + " ");
            }
            Console.WriteLine();

            // Ordenando a lista de números em ordem crescente
            numeros.Sort();

            // Imprimindo a lista de números ordenada
            Console.WriteLine("Números ordenados:");
            foreach (int numero in numeros)
            {
                Console.Write(numero + " ");
            }
            Console.WriteLine();

            // Calculando a média dos números
            int soma = 0;
            foreach (int numero in numeros)
            {
                soma += numero;
            }
            double media = (double)soma / numeros.Count;

            // Imprimindo a média dos números
            Console.WriteLine("Média dos números: " + media);

            // Verificando se um número específico está presente na lista
            Console.Write("Digite um número para verificar se está presente na lista: ");
            int numeroVerificar = int.Parse(Console.ReadLine());
            if (numeros.Contains(numeroVerificar))
            {
                Console.WriteLine("O número " + numeroVerificar + " está presente na lista.");
            }
            else
            {
                Console.WriteLine("O número " + numeroVerificar + " não está presente na lista.");
            }

            Console.WriteLine("Pressione qualquer tecla para sair.");
            Console.ReadKey();
        }
    }
}
```

Este código cria uma lista de números aleatórios, os ordena em ordem crescente, calcula a média dos números e verifica se um número específico está presente na lista. Ele utiliza os conceitos de listas, laços de repetição, ordenação, cálculos matemáticos e entrada/saída de dados.