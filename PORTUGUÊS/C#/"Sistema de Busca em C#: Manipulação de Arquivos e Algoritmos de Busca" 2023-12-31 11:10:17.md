Claro! Aqui está um código complexo em C# que envolve manipulação de arquivos e algoritmos de busca:

```csharp
using System;
using System.IO;

namespace SistemaDeBusca
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bem-vindo ao Sistema de Busca!");
            Console.WriteLine("Digite o caminho do diretório a ser pesquisado:");

            string diretorio = Console.ReadLine();

            if (!Directory.Exists(diretorio))
            {
                Console.WriteLine("Diretório inválido. Encerrando o programa...");
                return;
            }

            Console.WriteLine("Digite a palavra-chave a ser buscada:");

            string palavraChave = Console.ReadLine();

            Console.WriteLine("Iniciando a busca...");

            int quantidadeArquivos = 0;
            int quantidadeOcorrencias = 0;

            BuscarArquivos(diretorio, palavraChave, ref quantidadeArquivos, ref quantidadeOcorrencias);

            Console.WriteLine("Busca concluída!");
            Console.WriteLine("Foram encontrados {0} arquivo(s) e {1} ocorrência(s) da palavra-chave '{2}'.", quantidadeArquivos, quantidadeOcorrencias, palavraChave);
        }

        static void BuscarArquivos(string diretorio, string palavraChave, ref int quantidadeArquivos, ref int quantidadeOcorrencias)
        {
            try
            {
                foreach (string arquivo in Directory.GetFiles(diretorio))
                {
                    quantidadeArquivos++;

                    using (StreamReader reader = new StreamReader(arquivo))
                    {
                        string conteudo = reader.ReadToEnd();

                        if (conteudo.Contains(palavraChave))
                        {
                            quantidadeOcorrencias++;
                            Console.WriteLine("'{0}' encontrado no arquivo: {1}", palavraChave, arquivo);
                        }
                    }
                }

                foreach (string subDiretorio in Directory.GetDirectories(diretorio))
                {
                    BuscarArquivos(subDiretorio, palavraChave, ref quantidadeArquivos, ref quantidadeOcorrencias);
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Um erro ocorreu durante a busca: " + ex.Message);
            }
        }
    }
}
```

Este código implementa um sistema de busca em C#. Ele solicita ao usuário o caminho de um diretório a ser pesquisado e uma palavra-chave a ser buscada nos arquivos contidos nesse diretório e em seus subdiretórios.

O programa começa verificando se o diretório fornecido é válido e, caso contrário, encerra a execução. Em seguida, inicia a busca pelos arquivos que correspondem ao critério dentro do diretório. 

A função `BuscarArquivos` é responsável por realizar a busca em cada arquivo do diretório e, recursivamente, em seus subdiretórios. Ela utiliza a classe `StreamReader` para ler o conteúdo de cada arquivo e verifica se a palavra-chave fornecida está presente. Caso a palavra-chave seja encontrada, o programa exibe uma mensagem na tela indicando o arquivo onde ela foi encontrada.

Ao final da busca, o programa exibe a quantidade total de arquivos analisados e a quantidade de ocorrências da palavra-chave encontradas.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais esclarecimentos, estou à disposição para ajudar.