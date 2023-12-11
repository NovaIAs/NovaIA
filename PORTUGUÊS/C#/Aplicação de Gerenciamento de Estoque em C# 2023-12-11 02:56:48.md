Claro! Abaixo está um código em C# que implementa uma aplicação de gerenciamento de estoque de uma loja fictícia. O código é extenso e inclui diferentes funcionalidades para demonstrar a versatilidade da linguagem.

```csharp
using System;
using System.Collections.Generic;

namespace GerenciamentoEstoque
{
    public class Produto
    {
        public int Id { get; set; }
        public string Nome { get; set; }
        public int Quantidade { get; set; }
        public decimal Preco { get; set; }

        public Produto(int id, string nome, int quantidade, decimal preco)
        {
            Id = id;
            Nome = nome;
            Quantidade = quantidade;
            Preco = preco;
        }

        public void ExibirDetalhes()
        {
            Console.WriteLine("ID: " + Id);
            Console.WriteLine("Nome: " + Nome);
            Console.WriteLine("Quantidade: " + Quantidade);
            Console.WriteLine("Preço: R$" + Preco);
        }
    }

    public class Estoque
    {
        private List<Produto> _produtos;

        public Estoque()
        {
            _produtos = new List<Produto>();
        }

        public void AdicionarProduto(Produto produto)
        {
            _produtos.Add(produto);
        }

        public bool RemoverProduto(int id)
        {
            Produto produto = _produtos.Find(p => p.Id == id);

            if (produto != null)
            {
                _produtos.Remove(produto);
                return true;
            }

            return false;
        }

        public void ExibirProdutos()
        {
            Console.WriteLine("----- Produtos em Estoque -----");

            foreach (var produto in _produtos)
            {
                produto.ExibirDetalhes();
                Console.WriteLine("--------------------------");
            }
        }

        public void AtualizarQuantidade(int id, int quantidade)
        {
            Produto produto = _produtos.Find(p => p.Id == id);

            if (produto != null)
            {
                produto.Quantidade = quantidade;
                Console.WriteLine("Quantidade atualizada com sucesso!");
            }
            else
            {
                Console.WriteLine("Produto não encontrado.");
            }
        }

        public void RealizarVenda(int id, int quantidadeVendida)
        {
            Produto produto = _produtos.Find(p => p.Id == id);

            if (produto != null)
            {
                if (produto.Quantidade >= quantidadeVendida)
                {
                    produto.Quantidade -= quantidadeVendida;
                    Console.WriteLine("Venda realizada com sucesso!");
                }
                else
                {
                    Console.WriteLine("Quantidade insuficiente em estoque.");
                }
            }
            else
            {
                Console.WriteLine("Produto não encontrado.");
            }
        }
    }

    public class Program
    {
        static void Main(string[] args)
        {
            Estoque estoque = new Estoque();

            Produto produto1 = new Produto(1, "Camiseta", 10, 39.90m);
            Produto produto2 = new Produto(2, "Calça jeans", 5, 99.90m);
            Produto produto3 = new Produto(3, "Tênis", 8, 199.90m);

            estoque.AdicionarProduto(produto1);
            estoque.AdicionarProduto(produto2);
            estoque.AdicionarProduto(produto3);

            Console.WriteLine("---- Gerenciamento de Estoque ----");
            Console.WriteLine("1 - Adicionar produto");
            Console.WriteLine("2 - Remover produto");
            Console.WriteLine("3 - Exibir produtos");
            Console.WriteLine("4 - Atualizar quantidade");
            Console.WriteLine("5 - Realizar venda");
            Console.WriteLine("0 - Sair");

            int opcao = -1;

            while (opcao != 0)
            {
                Console.WriteLine("\nDigite uma opção:");
                opcao = Convert.ToInt32(Console.ReadLine());

                switch (opcao)
                {
                    case 1:
                        Console.WriteLine("Digite o ID do produto:");
                        int id = Convert.ToInt32(Console.ReadLine());

                        Console.WriteLine("Digite o nome do produto:");
                        string nome = Console.ReadLine();

                        Console.WriteLine("Digite a quantidade do produto:");
                        int quantidade = Convert.ToInt32(Console.ReadLine());

                        Console.WriteLine("Digite o preço do produto:");
                        decimal preco = Convert.ToDecimal(Console.ReadLine());

                        Produto novoProduto = new Produto(id, nome, quantidade, preco);
                        estoque.AdicionarProduto(novoProduto);
                        Console.WriteLine("Produto adicionado com sucesso!");
                        break;

                    case 2:
                        Console.WriteLine("Digite o ID do produto a ser removido:");
                        int idRemover = Convert.ToInt32(Console.ReadLine());

                        bool removido = estoque.RemoverProduto(idRemover);

                        if (removido)
                        {
                            Console.WriteLine("Produto removido com sucesso!");
                        }
                        else
                        {
                            Console.WriteLine("Produto não encontrado.");
                        }
                        break;

                    case 3:
                        estoque.ExibirProdutos();
                        break;

                    case 4:
                        Console.WriteLine("Digite o ID do produto:");
                        int idAtualizar = Convert.ToInt32(Console.ReadLine());

                        Console.WriteLine("Digite a nova quantidade:");
                        int quantidadeAtualizar = Convert.ToInt32(Console.ReadLine());

                        estoque.AtualizarQuantidade(idAtualizar, quantidadeAtualizar);
                        break;

                    case 5:
                        Console.WriteLine("Digite o ID do produto:");
                        int idVenda = Convert.ToInt32(Console.ReadLine());

                        Console.WriteLine("Digite a quantidade vendida:");
                        int quantidadeVendida = Convert.ToInt32(Console.ReadLine());

                        estoque.RealizarVenda(idVenda, quantidadeVendida);
                        break;

                    case 0:
                        Console.WriteLine("Saindo...");
                        break;

                    default:
                        Console.WriteLine("Opção inválida.");
                        break;
                }
            }
        }
    }
}
```

Este código implementa um sistema básico de gerenciamento de estoque onde é possível adicionar, remover, atualizar a quantidade e realizar vendas de produtos. A classe `Produto` define os atributos de cada produto, como seu ID, nome, quantidade e preço. A classe `Estoque` gerencia uma lista de produtos, permitindo adicionar, remover e atualizar informações. Além disso, possui métodos para exibir todos os produtos e realizar vendas.

No método `Main`, é criada uma instância da classe `Estoque` e alguns produtos são adicionados manualmente. Em seguida, é exibido um menu com opções para interagir com o estoque. O usuário pode escolher uma opção digitando o número correspondente. Cada opção executa um bloco de código diferente para realizar a ação desejada.

Espero que este código atenda às suas expectativas!