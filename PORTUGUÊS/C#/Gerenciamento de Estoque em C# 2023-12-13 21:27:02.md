Certamente! Aqui está um exemplo de um código complexo em C#, que envolve uma aplicação de gerenciamento de estoque de uma loja. Vou explicar o código linha por linha para que você possa entender melhor.

```csharp
using System;
using System.Collections.Generic;

namespace GerenciamentoEstoque
{
    class Program
    {
        static void Main(string[] args)
        {
            // Criação da lista de produtos
            List<Produto> estoque = new List<Produto>();

            // Adicionando alguns produtos ao estoque
            estoque.Add(new Produto("Notebook", 1499.99, 10));
            estoque.Add(new Produto("Smartphone", 799.99, 20));
            estoque.Add(new Produto("Headset", 99.99, 15));

            // Exibindo o menu principal
            ExibirMenuPrincipal(estoque);
        }

        static void ExibirMenuPrincipal(List<Produto> estoque)
        {
            while (true)
            {
                Console.Clear();
                Console.WriteLine("=== GERENCIAMENTO DE ESTOQUE ===");
                Console.WriteLine("1. Visualizar estoque");
                Console.WriteLine("2. Adicionar produto");
                Console.WriteLine("3. Remover produto");
                Console.WriteLine("4. Sair");
                Console.WriteLine("===============================");
                Console.Write("Selecione uma opção: ");

                string opcao = Console.ReadLine();

                switch (opcao)
                {
                    case "1":
                        ExibirEstoque(estoque);
                        break;
                    case "2":
                        AdicionarProduto(estoque);
                        break;
                    case "3":
                        RemoverProduto(estoque);
                        break;
                    case "4":
                        Environment.Exit(0);
                        break;
                    default:
                        Console.WriteLine("Opção inválida. Pressione qualquer tecla para continuar.");
                        Console.ReadKey();
                        break;
                }
            }
        }

        static void ExibirEstoque(List<Produto> estoque)
        {
            Console.Clear();
            Console.WriteLine("=== ESTOQUE ===");
            
            if (estoque.Count == 0)
            {
                Console.WriteLine("Nenhum produto encontrado no estoque.");
            }
            else
            {
                foreach (var produto in estoque)
                {
                    Console.WriteLine($"- {produto.Nome} | Preço: R${produto.Preco} | Quantidade: {produto.Quantidade}");
                }
            }

            Console.WriteLine("================");
            Console.WriteLine("Pressione qualquer tecla para voltar ao menu principal.");
            Console.ReadKey();
        }

        static void AdicionarProduto(List<Produto> estoque)
        {
            Console.Clear();
            Console.WriteLine("=== ADICIONAR PRODUTO ===");
            Console.Write("Digite o nome do produto: ");
            string nome = Console.ReadLine();

            Console.Write("Digite o preço do produto: ");
            double preco = Convert.ToDouble(Console.ReadLine());

            Console.Write("Digite a quantidade do produto: ");
            int quantidade = Convert.ToInt32(Console.ReadLine());

            estoque.Add(new Produto(nome, preco, quantidade));

            Console.WriteLine("Produto adicionado com sucesso!");
            Console.WriteLine("Pressione qualquer tecla para voltar ao menu principal.");
            Console.ReadKey();
        }

        static void RemoverProduto(List<Produto> estoque)
        {
            Console.Clear();
            Console.WriteLine("=== REMOVER PRODUTO ===");
            Console.Write("Digite o nome do produto a ser removido: ");
            string nome = Console.ReadLine();

            Produto produtoRemovido = estoque.Find(produto => produto.Nome.ToLower() == nome.ToLower());

            if (produtoRemovido != null)
            {
                estoque.Remove(produtoRemovido);
                Console.WriteLine("Produto removido com sucesso!");
            }
            else
            {
                Console.WriteLine("Produto não encontrado no estoque.");
            }

            Console.WriteLine("Pressione qualquer tecla para voltar ao menu principal.");
            Console.ReadKey();
        }
    }

    class Produto
    {
        public string Nome { get; set; }
        public double Preco { get; set; }
        public int Quantidade { get; set; }

        public Produto(string nome, double preco, int quantidade)
        {
            Nome = nome;
            Preco = preco;
            Quantidade = quantidade;
        }
    }
}
```

Neste código, temos as seguintes funcionalidades:

- Criação de uma classe `Produto`, que representa um produto do estoque, com propriedades como nome, preço e quantidade.
- A classe `Program` contém o método `Main`, que é o ponto de entrada do programa.
- O método `ExibirMenuPrincipal` exibe o menu principal para o usuário e permite selecionar diversas opções, como visualizar o estoque, adicionar um produto, remover um produto ou sair do programa.
- O método `ExibirEstoque` exibe todos os produtos presentes no estoque.
- O método `AdicionarProduto` permite ao usuário adicionar um novo produto ao estoque, solicitando o nome, preço e quantidade do produto.
- O método `RemoverProduto` permite ao usuário remover um produto do estoque com base no nome fornecido.
- A aplicação utiliza uma lista chamada `estoque` para armazenar os produtos.

Espero que isso te ajude a entender o código em C#! Se você tiver mais alguma dúvida ou precisar de ajuda com qualquer outra coisa, fique à vontade para perguntar!