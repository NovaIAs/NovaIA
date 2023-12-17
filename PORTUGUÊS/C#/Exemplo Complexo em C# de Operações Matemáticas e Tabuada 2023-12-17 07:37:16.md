Claro! Aqui está um exemplo de código complexo em C#:

```csharp
using System;

namespace ExemploComplexo
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bem-vindo ao Exemplo Complexo!");

            // Declaração de variáveis
            int numero1, numero2;
            double resultado;

            // Solicitar números ao usuário
            Console.Write("Digite o primeiro número: ");
            numero1 = int.Parse(Console.ReadLine());

            Console.Write("Digite o segundo número: ");
            numero2 = int.Parse(Console.ReadLine());

            // Realizar operações matemáticas
            resultado = Somar(numero1, numero2);
            Console.WriteLine($"A soma dos números é: {resultado}");

            resultado = Subtrair(numero1, numero2);
            Console.WriteLine($"A subtração dos números é: {resultado}");

            resultado = Multiplicar(numero1, numero2);
            Console.WriteLine($"A multiplicação dos números é: {resultado}");

            resultado = Dividir(numero1, numero2);
            Console.WriteLine($"A divisão dos números é: {resultado}");

            // Verificar se número1 é maior, menor ou igual a número2
            VerificarMaiorMenorIgual(numero1, numero2);

            // Mostrar a tabuada do número1
            MostrarTabuada(numero1);

            Console.WriteLine("Obrigado por utilizar o Exemplo Complexo!");
        }

        static double Somar(int num1, int num2)
        {
            return num1 + num2;
        }

        static double Subtrair(int num1, int num2)
        {
            return num1 - num2;
        }

        static double Multiplicar(int num1, int num2)
        {
            return num1 * num2;
        }

        static double Dividir(int num1, int num2)
        {
            if (num2 != 0)
                return (double)num1 / num2;
            else
            {
                Console.WriteLine("Não é possível dividir por zero!");
                return 0;
            }
        }

        static void VerificarMaiorMenorIgual(int num1, int num2)
        {
            if (num1 > num2)
                Console.WriteLine($"{num1} é maior que {num2}");
            else if (num1 < num2)
                Console.WriteLine($"{num1} é menor que {num2}");
            else
                Console.WriteLine($"{num1} é igual a {num2}");
        }

        static void MostrarTabuada(int num)
        {
            Console.WriteLine($"Tabuada do {num}:");
            for (int i = 1; i <= 10; i++)
            {
                int resultado = num * i;
                Console.WriteLine($"{num} x {i} = {resultado}");
            }
        }
    }
}
```

Este código é um exemplo complexo em C# que demonstra a utilização de várias funcionalidades da linguagem. Ele permite ao usuário inserir dois números e, em seguida, realiza as operações matemáticas básicas (soma, subtração, multiplicação e divisão) entre esses números.

Além disso, o código também verifica se o primeiro número é maior, menor ou igual ao segundo número e mostra a tabuada do primeiro número.

Cada funcionalidade foi implementada em um método separado para tornar o código mais modular e fácil de entender. Os métodos `Somar`, `Subtrair`, `Multiplicar` e `Dividir` realizam as operações matemáticas. O método `VerificarMaiorMenorIgual` verifica a relação entre os números. O método `MostrarTabuada` exibe a tabuada do número fornecido.

Espero que este exemplo complexo seja útil para você!