Claro! Aqui está um exemplo de um código complexo em C# que envolve manipulação de strings e estruturas de repetição:

```csharp
using System;

namespace ExemploComplexo
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Digite uma frase:");
            string frase = Console.ReadLine();

            Console.WriteLine("\nAnalisando a frase...");

            int totalPalavras = ContarPalavras(frase);
            int totalVogais = ContarVogais(frase);
            int totalConsoantes = ContarConsoantes(frase);

            Console.WriteLine("\nResultado da análise:");
            Console.WriteLine("Total de palavras: " + totalPalavras);
            Console.WriteLine("Total de vogais: " + totalVogais);
            Console.WriteLine("Total de consoantes: " + totalConsoantes);
        }

        static int ContarPalavras(string frase)
        {
            int contador = 0;
            string[] palavras = frase.Split(' ');

            foreach (string palavra in palavras)
            {
                if (!string.IsNullOrWhiteSpace(palavra))
                {
                    contador++;
                }
            }

            return contador;
        }

        static int ContarVogais(string frase)
        {
            int contador = 0;
            string vogais = "AEIOUÁÀÃÂÉÈÊÍÌÎÓÒÔÕÚÙÛ";

            foreach (char letra in frase.ToUpper())
            {
                if (vogais.Contains(letra.ToString()))
                {
                    contador++;
                }
            }

            return contador;
        }

        static int ContarConsoantes(string frase)
        {
            int contador = 0;
            string consoantes = "BCDFGHJKLMNPQRSTVWXYZÇ";

            foreach (char letra in frase.ToUpper())
            {
                if (consoantes.Contains(letra.ToString()))
                {
                    contador++;
                }
            }

            return contador;
        }
    }
}
```

Neste código, é solicitado ao usuário que digite uma frase. Em seguida, a frase é analisada e três informações são exibidas: o total de palavras, o total de vogais e o total de consoantes presentes na frase.

A função `ContarPalavras` recebe a frase informada pelo usuário e realiza a contagem de palavras utilizando o método `Split`, que divide a frase em um array de strings a cada espaço em branco encontrado. Em seguida, é feita uma iteração pelo array resultante, e caso uma palavra não seja nula ou composta apenas por espaços em branco, o contador é incrementado.

A função `ContarVogais` recebe a frase informada pelo usuário e realiza a contagem de vogais. É definido um string contendo todas as vogais maiúsculas e acentuadas. Em seguida, é feita uma iteração por cada caractere da frase convertido para maiúsculo. Caso o caractere esteja contido na lista de vogais, o contador é incrementado.

A função `ContarConsoantes` é semelhante à `ContarVogais`, porém realiza a contagem de consoantes. É definido um string contendo todas as consoantes maiúsculas e a letra "Ç". Em seguida, é feita uma iteração por cada caractere da frase convertido para maiúsculo. Caso o caractere esteja contido na lista de consoantes, o contador é incrementado.

No método `Main`, os resultados das análises são exibidos na tela.