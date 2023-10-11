Claro! Vou gerar um código complexo em C# para você. Este código é um exemplo de um sistema de gerenciamento de estoque de uma loja. Ele permite adicionar, remover e atualizar produtos no estoque, além de exibir relatórios e buscar produtos por nome.

```csharp
using System;
using System.Collections.Generic;

namespace GerenciamentoEstoque
{
    class Program
    {
        static List<Produto> estoque = new List<Produto>();

        static void Main(string[] args)
        {
            bool executando = true;

            while (executando)
            {
                Console.Clear();
                Console.WriteLine("===== Sistema de Gerenciamento de Estoque =====");
                Console.WriteLine("1- Adicionar Produto");
                Console.WriteLine("2- Remover Produto");
                Console.WriteLine("3- Atualizar Produto");
                Console.WriteLine("4- Relatório de Estoque");
                Console.WriteLine("5- Buscar Produto por Nome");
                Console.WriteLine("6- Sair");
                Console.WriteLine("==============================================");
                Console.Write("Digite a opção desejada: ");
                string opcao = Console.ReadLine();

                switch (opcao)
                {
                    case "1":
                        AdicionarProduto();
                        break;
                    case "2":
                        RemoverProduto();
                        break;
                    case "3":
                        AtualizarProduto();
                        break;
                    case "4":
                        ExibirRelatorioEstoque();
                        break;
                    case "5":
                        BuscarProdutoPorNome();
                        break;
                    case "6":
                        executando = false;
                        break;
                    default:
                        Console.WriteLine("Opção inválida. Pressione qualquer tecla para continuar...");
                        Console.ReadKey();
                        break;
                }
            }
        }

        static void AdicionarProduto()
        {
            Console.Clear();
            Console.WriteLine("===== Adicionar Produto ao Estoque =====");
            Console.Write("Digite o nome do produto: ");
            string nome = Console.ReadLine();
            Console.Write("Digite a quantidade: ");
            int quantidade = int.Parse(Console.ReadLine());
            Console.Write("Digite o preço unitário: ");
            double preco = double.Parse(Console.ReadLine());

            Produto produto = new Produto(nome, quantidade, preco);
            estoque.Add(produto);

            Console.WriteLine("\nProduto adicionado com sucesso! Pressione qualquer tecla para continuar...");
            Console.ReadKey();
        }

        static void RemoverProduto()
        {
            Console.Clear();
            Console.WriteLine("===== Remover Produto do Estoque =====");
            Console.Write("Digite o nome do produto: ");
            string nome = Console.ReadLine();

            Produto produto = estoque.Find(p => p.Nome.ToLower() == nome.ToLower());

            if (produto != null)
            {
                estoque.Remove(produto);
                Console.WriteLine("\nProduto removido com sucesso! Pressione qualquer tecla para continuar...");
            }
            else
            {
                Console.WriteLine("\nProduto não encontrado! Pressione qualquer tecla para continuar...");
            }

            Console.ReadKey();
        }

        static void AtualizarProduto()
        {
            Console.Clear();
            Console.WriteLine("===== Atualizar Produto do Estoque =====");
            Console.Write("Digite o nome do produto: ");
            string nome = Console.ReadLine();

            Produto produto = estoque.Find(p => p.Nome.ToLower() == nome.ToLower());

            if (produto != null)
            {
                Console.Write("Digite a nova quantidade: ");
                int quantidade = int.Parse(Console.ReadLine());
                Console.Write("Digite o novo preço unitário: ");
                double preco = double.Parse(Console.ReadLine());

                produto.Quantidade = quantidade;
                produto.Preco = preco;

                Console.WriteLine("\nProduto atualizado com sucesso! Pressione qualquer tecla para continuar...");
            }
            else
            {
                Console.WriteLine("\nProduto não encontrado! Pressione qualquer tecla para continuar...");
            }

            Console.ReadKey();
        }

        static void ExibirRelatorioEstoque()
        {
            Console.Clear();
            Console.WriteLine("===== Relatório de Estoque =====");
            Console.WriteLine("Nome\t\tQuantidade\tPreço Unitário");
            Console.WriteLine("--------------------------------");

            foreach (Produto produto in estoque)
            {
                Console.WriteLine($"{produto.Nome}\t\t{produto.Quantidade}\t\t{produto.Preco:C}");
            }

            Console.WriteLine("\nPressione qualquer tecla para continuar...");
            Console.ReadKey();
        }

        static void BuscarProdutoPorNome()
        {
            Console.Clear();
            Console.WriteLine("===== Buscar Produto por Nome =====");
            Console.Write("Digite o nome do produto: ");
            string nome = Console.ReadLine();

            List<Produto> produtosEncontrados = estoque.FindAll(p => p.Nome.ToLower().Contains(nome.ToLower()));

            if (produtosEncontrados.Count > 0)
            {
                Console.WriteLine("\nProdutos encontrados:");
                Console.WriteLine("Nome\t\tQuantidade\tPreço Unitário");
                Console.WriteLine("--------------------------------");

                foreach (Produto produto in produtosEncontrados)
                {
                    Console.WriteLine($"{produto.Nome}\t\t{produto.Quantidade}\t\t{produto.Preco:C}");
                }
            }
            else
            {
                Console.WriteLine("\nNenhum produto encontrado!");
            }

            Console.WriteLine("\nPressione qualquer tecla para continuar...");
            Console.ReadKey();
        }
    }

    class Produto
    {
        public string Nome { get; set; }
        public int Quantidade { get; set; }
        public double Preco { get; set; }

        public Produto(string nome, int quantidade, double preco)
        {
            Nome = nome;
            Quantidade = quantidade;
            Preco = preco;
        }
    }
}
```

Este código apresenta um menu de opções para o usuário interagir com o sistema. Ele permite adicionar produtos ao estoque, remover produtos, atualizar informações de produtos, exibir um relatório de estoque e buscar produtos por nome.

O estoque é uma lista de objetos da classe `Produto`, que possui propriedades para o nome, quantidade e preço do produto. O código utiliza as estruturas de controle `while` e `switch` para executar as operações escolhidas pelo usuário.

Na função `AdicionarProduto`, o usuário informa o nome, quantidade e preço do produto, e um novo objeto `Produto` é criado e adicionado ao estoque.

Na função `RemoverProduto`, o usuário informa o nome do produto que deseja remover, e o objeto correspondente é buscado na lista e removido, se encontrado.

A função `AtualizarProduto` permite ao usuário atualizar a quantidade e o preço de um produto existente no estoque. O usuário informa o nome do produto que deseja atualizar, e as novas informações são solicitadas.

A função `ExibirRelatorioEstoque` exibe todos os produtos presentes no estoque, mostrando o nome, quantidade e preço unitário de cada um.

Por fim, a função `BuscarProdutoPorNome` permite ao usuário buscar produtos no estoque pelo nome. O usuário informa o nome (ou parte do nome) do produto desejado, e todos os produtos que correspondem à busca são exibidos.

Espero que este código complexo em C# atenda às suas expectativas!