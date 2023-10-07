Claro! Aqui está um exemplo de um código complexo em C# que cria uma calculadora simples de console:

```csharp
using System;

namespace Calculadora
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bem-vindo à Calculadora!");
            Console.WriteLine("Digite o primeiro número:");
            double num1 = double.Parse(Console.ReadLine());

            Console.WriteLine("Digite o segundo número:");
            double num2 = double.Parse(Console.ReadLine());

            Console.WriteLine("Escolha uma operação:");
            Console.WriteLine("1. Adição");
            Console.WriteLine("2. Subtração");
            Console.WriteLine("3. Multiplicação");
            Console.WriteLine("4. Divisão");
            int operacao = int.Parse(Console.ReadLine());

            double resultado = 0;
            string operador = "";

            switch (operacao)
            {
                case 1:
                    resultado = num1 + num2;
                    operador = "+";
                    break;
                case 2:
                    resultado = num1 - num2;
                    operador = "-";
                    break;
                case 3:
                    resultado = num1 * num2;
                    operador = "*";
                    break;
                case 4:
                    if (num2 != 0)
                    {
                        resultado = num1 / num2;
                        operador = "/";
                    }
                    else
                    {
                        Console.WriteLine("Não é possível dividir por zero!");
                    }
                    break;
                default:
                    Console.WriteLine("Operação inválida!");
                    break;
            }

            if (operador != "")
            {
                Console.WriteLine($"O resultado de {num1} {operador} {num2} é {resultado}");
            }

            Console.WriteLine("Pressione qualquer tecla para sair...");
            Console.ReadKey();
        }
    }
}
```

Este código implementa uma calculadora simples de console. Ele solicita ao usuário dois números e, em seguida, oferece uma lista de operações para escolher. Após a seleção da operação, o código realiza o cálculo e exibe o resultado na tela.

O programa começa exibindo uma mensagem de boas-vindas e, em seguida, solicita ao usuário que digite o primeiro número. Ele armazena esse valor na variável `num1` após convertê-lo para o tipo `double` usando o método `Parse` da classe `double`.

Em seguida, o programa pede ao usuário para digitar o segundo número e armazena o valor na variável `num2`, novamente convertendo-o para `double`.

Após a entrada dos números, o programa exibe uma lista de operações para o usuário escolher. Ele lê a escolha do usuário e armazena o valor na variável `operacao`.

Em seguida, o programa usa um switch case para realizar a operação selecionada. Dependendo do valor de `operacao`, o código realizará as operações de adição, subtração, multiplicação ou divisão. O resultado é armazenado na variável `resultado` e o operador correspondente é armazenado na variável `operador`.

Se a operação selecionada for a divisão (opção 4), o código verifica se o segundo número é diferente de zero antes de realizar a divisão. Se for zero, exibe uma mensagem informando que a divisão por zero não é possível.

Após realizar a operação, o programa verifica se o operador não está vazio (ou seja, se a operação selecionada não foi a divisão por zero) e, em seguida, exibe o resultado na tela.

Finalmente, o programa solicita ao usuário que pressione qualquer tecla para sair, usando o método `ReadKey` da classe `Console`.

Espero que este exemplo atenda às suas expectativas de um código complexo e diferenciado em C#! Se você tiver alguma dúvida, por favor, me avise.