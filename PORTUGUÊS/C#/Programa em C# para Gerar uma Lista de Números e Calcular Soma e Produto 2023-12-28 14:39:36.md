Claro! Aqui está um exemplo de um código complexo em C#:

```csharp
using System;
using System.Collections.Generic;

namespace ExemploComplexo
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bem-vindo ao Exemplo Complexo!");

            Console.WriteLine("Digite um número inteiro:");
            int numero = int.Parse(Console.ReadLine());

            List<int> listaNumeros = GerarListaNumeros(numero);
            Console.WriteLine("A lista de números gerada é: ");
            foreach (int num in listaNumeros)
            {
                Console.Write(num + " ");
            }
            Console.WriteLine();

            int somaPares = CalcularSomaPares(listaNumeros);
            Console.WriteLine("A soma dos números pares da lista é: " + somaPares);

            int produtoImpares = CalcularProdutoImpares(listaNumeros);
            Console.WriteLine("O produto dos números ímpares da lista é: " + produtoImpares);

            Console.ReadKey();
        }

        static List<int> GerarListaNumeros(int limite)
        {
            List<int> lista = new List<int>();
            for (int i = 1; i <= limite; i++)
            {
                lista.Add(i);
            }
            return lista;
        }

        static int CalcularSomaPares(List<int> lista)
        {
            int soma = 0;
            foreach (int num in lista)
            {
                if (num % 2 == 0)
                {
                    soma += num;
                }
            }
            return soma;
        }

        static int CalcularProdutoImpares(List<int> lista)
        {
            int produto = 1;
            foreach (int num in lista)
            {
                if (num % 2 != 0)
                {
                    produto *= num;
                }
            }
            return produto;
        }
    }
}
```

Neste exemplo, criei um programa em C# que permite ao usuário digitar um número inteiro. Em seguida, gera uma lista de números de 1 até o número digitado. Em seguida, o programa calcula a soma dos números pares e o produto dos números ímpares da lista gerada. 

Para isso, criei três métodos: 

- O método `GerarListaNumeros` recebe um número inteiro como parâmetro e retorna uma lista de números de 1 até o valor passado.
- O método `CalcularSomaPares` recebe uma lista de inteiros como parâmetro e calcula a soma dos números pares presentes na lista.
- O método `CalcularProdutoImpares` recebe uma lista de inteiros como parâmetro e calcula o produto dos números ímpares presentes na lista.

No método `Main`, o programa inicia mostrando uma mensagem de boas-vindas. Em seguida, solicita ao usuário que digite um número inteiro. Após receber a entrada do usuário, chama o método `GerarListaNumeros` para criar a lista de números. Depois, exibe a lista na tela. Em seguida, chama os métodos `CalcularSomaPares` e `CalcularProdutoImpares` para calcular a soma dos números pares e o produto dos números ímpares, respectivamente. Por fim, exibe os resultados na tela antes de aguardar que o usuário pressione qualquer tecla para encerrar o programa.