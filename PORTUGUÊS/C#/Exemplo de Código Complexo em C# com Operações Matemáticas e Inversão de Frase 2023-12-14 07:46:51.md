Claro! Aqui está um exemplo de código complexo em C#, com explicações detalhadas:

```csharp
using System;

namespace ExemploComplexo
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bem-vindo ao Exemplo Complexo!");

            Console.WriteLine("Digite um número inteiro:");
            int numero = Convert.ToInt32(Console.ReadLine());

            int resultado = CalcularFatorial(numero);
            Console.WriteLine($"O fatorial de {numero} é {resultado}.");

            Console.WriteLine("Digite um número decimal:");
            double valor = Convert.ToDouble(Console.ReadLine());

            double resultadoPotencia = CalcularPotencia(valor);
            Console.WriteLine($"O resultado da potência é {resultadoPotencia}.");

            Console.WriteLine("Digite uma frase:");
            string frase = Console.ReadLine();

            string fraseInvertida = InverterFrase(frase);
            Console.WriteLine($"A frase invertida é: {fraseInvertida}.");

            Console.WriteLine("Pressione qualquer tecla para sair.");
            Console.ReadKey();
        }

        static int CalcularFatorial(int n)
        {
            if (n == 0)
                return 1;
            else
                return n * CalcularFatorial(n - 1);
        }

        static double CalcularPotencia(double x)
        {
            return Math.Pow(x, 2);
        }

        static string InverterFrase(string frase)
        {
            char[] caracteres = frase.ToCharArray();
            Array.Reverse(caracteres);
            return new string(caracteres);
        }
    }
}
```

Neste exemplo, temos um programa em C# que realiza três operações diferentes:

1. Cálculo do fatorial de um número inteiro.
   - A função `CalcularFatorial` utiliza recursão para calcular o fatorial de um número. Se o número digitado for igual a 0, a função retorna 1. Caso contrário, ela chama a si mesma com o número decrementado e multiplica pelo número atual.
   - O número digitado pelo usuário é armazenado na variável `numero`.
   - O resultado do cálculo do fatorial é armazenado na variável `resultado` e exibido na tela.

2. Cálculo da potência de um número decimal.
   - A função `CalcularPotencia` utiliza o método `Math.Pow` para calcular a potência de um número. Neste exemplo, elevamos o número digitado pelo usuário ao quadrado.
   - O número digitado pelo usuário é armazenado na variável `valor`.
   - O resultado do cálculo da potência é armazenado na variável `resultadoPotencia` e exibido na tela.

3. Inversão de uma frase.
   - A função `InverterFrase` converte a frase em um array de caracteres utilizando `ToCharArray`. Em seguida, utiliza o método `Array.Reverse` para inverter a ordem dos caracteres.
   - A frase digitada pelo usuário é armazenada na variável `frase`.
   - O resultado da inversão da frase é armazenado na variável `fraseInvertida` e exibido na tela.

Após a execução das três operações, o programa aguarda a pressionar qualquer tecla para encerrar.