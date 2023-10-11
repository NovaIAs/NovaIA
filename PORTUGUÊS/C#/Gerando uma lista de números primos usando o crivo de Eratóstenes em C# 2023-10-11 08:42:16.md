Claro! Segue um exemplo de código complexo em C# que cria uma lista de números primos usando o crivo de Eratóstenes:

```csharp
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        Console.WriteLine("Digite o número máximo para gerar a lista de números primos: ");
        int max = Convert.ToInt32(Console.ReadLine());

        List<int> primos = GerarListaPrimos(max);

        Console.WriteLine("Lista de números primos até o número {0}:", max);
        foreach (int primo in primos)
        {
            Console.Write(primo + " ");
        }

        Console.ReadLine();
    }

    static List<int> GerarListaPrimos(int max)
    {
        bool[] crivo = new bool[max + 1];
        List<int> primos = new List<int>();

        for (int i = 2; i <= max; i++)
        {
            if (!crivo[i])
            {
                primos.Add(i);

                for (int j = i * 2; j <= max; j += i)
                {
                    crivo[j] = true;
                }
            }
        }

        return primos;
    }
}
```

Este código solicita ao usuário um número máximo e, em seguida, gera uma lista de números primos até esse número utilizando o crivo de Eratóstenes.

A função `GerarListaPrimos(int max)` recebe o número máximo como parâmetro e retorna uma lista contendo todos os números primos encontrados. Ela utiliza um array booleano chamado `crivo` para marcar os números que não são primos. Inicialmente, todos os valores do `crivo` são definidos como `false`. Em seguida, um loop passa por todos os números de 2 até o número máximo fornecido. Se um número não estiver marcado como `true` no `crivo`, ele é considerado primo e adicionado à lista de primos. Depois, o loop interno marca todos os múltiplos desse número como `true`, indicando que não são primos.

No método `Main()`, o código solicita ao usuário que insira um número máximo. Em seguida, chama a função `GerarListaPrimos()` para obter a lista de primos e imprime os resultados na tela.

Esse código é capaz de gerar uma lista de números primos de maneira eficiente, evitando a verificação repetida de números não primos. É uma implementação útil e complexa que pode ser utilizada em diversas aplicações que envolvam números primos.