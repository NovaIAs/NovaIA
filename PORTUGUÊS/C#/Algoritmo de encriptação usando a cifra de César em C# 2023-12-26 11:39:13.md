Claro! Aqui está um código complexo em C# que realiza um algoritmo de encriptação usando a cifra de César. A cifra de César é um tipo de encriptação que substitui cada letra do texto original por uma letra deslocada um certo número de posições no alfabeto.

```csharp
using System;

namespace CifraDeCesar
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Digite um texto para encriptar:");
            string textoOriginal = Console.ReadLine();

            Console.WriteLine("Digite o número de posições para deslocar o texto:");
            int deslocamento = int.Parse(Console.ReadLine());

            string textoEncriptado = EncriptarTexto(textoOriginal, deslocamento);
            Console.WriteLine("Texto encriptado: " + textoEncriptado);

            string textoDesencriptado = DesencriptarTexto(textoEncriptado, deslocamento);
            Console.WriteLine("Texto desencriptado: " + textoDesencriptado);
        }

        static string EncriptarTexto(string texto, int deslocamento)
        {
            string textoEncriptado = "";

            foreach (char letra in texto)
            {
                if (char.IsLetter(letra))
                {
                    char letraEncriptada = (char)(((int)letra + deslocamento - 65) % 26 + 65);
                    textoEncriptado += letraEncriptada;
                }
                else
                {
                    textoEncriptado += letra;
                }
            }

            return textoEncriptado;
        }

        static string DesencriptarTexto(string texto, int deslocamento)
        {
            string textoDesencriptado = "";

            foreach (char letra in texto)
            {
                if (char.IsLetter(letra))
                {
                    char letraDesencriptada = (char)(((int)letra - deslocamento - 65 + 26) % 26 + 65);
                    textoDesencriptado += letraDesencriptada;
                }
                else
                {
                    textoDesencriptado += letra;
                }
            }

            return textoDesencriptado;
        }
    }
}
```

Neste código, a função `Main` solicita ao usuário um texto para ser encriptado e o número de posições para deslocar o texto. Em seguida, chama as funções `EncriptarTexto` e `DesencriptarTexto` para realizar a encriptação e desencriptação, respectivamente.

A função `EncriptarTexto` itera por cada letra do texto original e verifica se é uma letra do alfabeto. Caso seja, realiza o deslocamento utilizando a tabela ASCII. Em seguida, adiciona a letra encriptada ao texto encriptado.

A função `DesencriptarTexto` funciona da mesma forma que `EncriptarTexto`, porém realiza o deslocamento inverso para desencriptar o texto.

O código completo é capaz de encriptar e desencriptar textos usando a cifra de César.