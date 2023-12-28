Claro! Aqui está um exemplo de código complexo em C# que realiza uma operação matemática de forma iterativa, utilizando recursão de cauda:

```csharp
using System;

namespace ExemploRecursaoCauda
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Digite um número inteiro positivo:");
            int numero = int.Parse(Console.ReadLine());

            int resultado = CalcularFatorial(numero);
            Console.WriteLine($"O fatorial de {numero} é {resultado}.");

            Console.ReadLine();
        }

        static int CalcularFatorial(int n)
        {
            return CalcularFatorialAuxiliar(n, 1);
        }

        static int CalcularFatorialAuxiliar(int n, int fatorial)
        {
            if (n == 0)
                return fatorial;
            
            return CalcularFatorialAuxiliar(n - 1, n * fatorial);
        }
    }
}
```

Neste código, pedimos ao usuário para digitar um número inteiro positivo. Em seguida, chamamos a função `CalcularFatorial`, que é responsável por calcular o fatorial desse número utilizando recursão de cauda.

A função `CalcularFatorial` recebe como parâmetro o número digitado pelo usuário e retorna o resultado do fatorial. Dentro dessa função, chamamos a função `CalcularFatorialAuxiliar`, passando o número digitado, juntamente com o fatorial inicializado com o valor 1.

A função `CalcularFatorialAuxiliar` é a função recursiva propriamente dita. Ela recebe como parâmetros o número atual e o fatorial parcial. A cada chamada recursiva, o número é decrementado em 1 e o fatorial é multiplicado pelo número atual. Essa recursão ocorre até que o número seja igual a 0, momento em que o fatorial final é retornado.

O resultado é então exibido na tela. Neste exemplo, utilizamos o cálculo de fatorial apenas como exemplo de uma operação matemática complexa, mas o conceito de recursão de cauda pode ser aplicado a uma variedade de problemas.