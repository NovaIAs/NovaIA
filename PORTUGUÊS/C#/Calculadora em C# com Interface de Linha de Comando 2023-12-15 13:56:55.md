Claro! A seguir está um exemplo de código complexo em C# que envolve a criação de uma calculadora com uma interface de linha de comando:

```csharp
using System;

namespace Calculadora
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bem-vindo à Calculadora!");

            while (true)
            {
                Console.WriteLine("Escolha uma operação:");
                Console.WriteLine("1. Soma");
                Console.WriteLine("2. Subtração");
                Console.WriteLine("3. Multiplicação");
                Console.WriteLine("4. Divisão");
                Console.WriteLine("5. Sair");

                int escolha = Convert.ToInt32(Console.ReadLine());

                if (escolha == 5)
                {
                    Console.WriteLine("Obrigado por usar a Calculadora. Até mais!");
                    break;
                }

                Console.WriteLine("Digite o primeiro número:");
                double num1 = Convert.ToDouble(Console.ReadLine());

                Console.WriteLine("Digite o segundo número:");
                double num2 = Convert.ToDouble(Console.ReadLine());

                double resultado = 0;

                switch (escolha)
                {
                    case 1:
                        resultado = num1 + num2;
                        Console.WriteLine("O resultado da soma é: " + resultado);
                        break;
                    case 2:
                        resultado = num1 - num2;
                        Console.WriteLine("O resultado da subtração é: " + resultado);
                        break;
                    case 3:
                        resultado = num1 * num2;
                        Console.WriteLine("O resultado da multiplicação é: " + resultado);
                        break;
                    case 4:
                        if (num2 != 0)
                        {
                            resultado = num1 / num2;
                            Console.WriteLine("O resultado da divisão é: " + resultado);
                        }
                        else
                        {
                            Console.WriteLine("Divisão por zero não é permitida!");
                        }
                        break;
                    default:
                        Console.WriteLine("Opção inválida! Por favor, escolha uma operação válida.");
                        break;
                }
            }
        }
    }
}
```

Esse código cria uma calculadora simples em C# que permite ao usuário escolher entre diferentes operações matemáticas (soma, subtração, multiplicação, divisão) ou sair do programa. Ele faz uso de um loop infinito para permitir que o usuário execute várias operações consecutivamente até escolher sair.

O código começa exibindo uma mensagem de boas-vindas e, em seguida, apresenta um menu com as opções disponíveis. O usuário pode escolher uma operação digitando o número correspondente. Em seguida, o código solicita que o usuário insira os dois números necessários para realizar a operação escolhida.

Depois de obter os números, o código utiliza uma instrução switch para determinar qual operação foi escolhida e realiza o cálculo necessário. O resultado é armazenado na variável "resultado" e, em seguida, exibido na tela.

O código também inclui tratamento de erro para evitar a divisão por zero. Se o usuário escolher a operação de divisão e o segundo número for zero, será exibida uma mensagem de erro.

O loop infinito continua até que o usuário escolha a opção de sair (opção 5), momento em que o programa exibe uma mensagem de despedida e encerra a execução.

Espero que esse código complexo atenda às suas expectativas!