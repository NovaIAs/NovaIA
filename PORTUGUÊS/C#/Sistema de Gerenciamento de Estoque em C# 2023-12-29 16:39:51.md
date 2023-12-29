Claro! Vou gerar um código complexo em C# para você. O código abaixo é um exemplo de um sistema de gerenciamento de estoque de uma loja:

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
            bool sair = false;

            while (!sair)
            {
                Console.WriteLine("----- Gerenciamento de Estoque -----");
                Console.WriteLine("1. Adicionar Produto");
                Console.WriteLine("2. Remover Produto");
                Console.WriteLine("3. Listar Estoque");
                Console.WriteLine("4. Sair");
                Console.WriteLine("-------------------------------------");
                Console.Write("Digite a opção desejada: ");
                int opcao = int.Parse(Console.ReadLine());

                switch (opcao)
                {
                    case 1:
                        AdicionarProduto();
                        break;
                    case 2:
                        RemoverProduto();
                        break;
                    case 3:
                        ListarEstoque();
                        break;
                    case 4:
                        sair = true;
                        break;
                    default:
                        Console.WriteLine("Opção inválida. Tente novamente.");
                        break;
                }

                Console.WriteLine();
            }
        }

        static void AdicionarProduto()
        {
            Console.WriteLine("----- Adicionar Produto -----");
            Console.Write("Digite o nome do produto: ");
            string nome = Console.ReadLine();
            Console.Write("Digite o preço do produto: ");
            double preco = double.Parse(Console.ReadLine());
            Console.Write("Digite a quantidade do produto: ");
            int quantidade = int.Parse(Console.ReadLine());

            Produto novoProduto = new Produto(nome, preco, quantidade);
            estoque.Add(novoProduto);

            Console.WriteLine("Produto adicionado com sucesso!");
        }

        static void RemoverProduto()
        {
            Console.WriteLine("----- Remover Produto -----");
            Console.Write("Digite o nome do produto: ");
            string nome = Console.ReadLine();

            Produto produtoRemovido = estoque.Find(p => p.Nome == nome);

            if (produtoRemovido != null)
            {
                estoque.Remove(produtoRemovido);
                Console.WriteLine("Produto removido com sucesso!");
            }
            else
            {
                Console.WriteLine("Produto não encontrado no estoque.");
            }
        }

        static void ListarEstoque()
        {
            Console.WriteLine("----- Estoque Atual -----");
            foreach (Produto produto in estoque)
            {
                Console.WriteLine($"Nome: {produto.Nome} | Preço: R${produto.Preco:N2} | Quantidade: {produto.Quantidade}");
            }
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

Neste código, temos uma classe `Produto`, que representa um produto do estoque, com propriedades como `Nome`, `Preco` e `Quantidade`. A classe `Program` é a classe principal, que contém o método `Main`, onde ocorre a interação com o usuário.

O programa oferece um menu com opções de adicionar produto, remover produto, listar estoque e sair. Ao selecionar uma opção, o usuário é solicitado a fornecer as informações necessárias, como nome, preço e quantidade do produto. Em seguida, as operações correspondentes são executadas.

O estoque é armazenado em uma lista de objetos da classe `Produto`. Para adicionar um produto, é criada uma instância dessa classe com os dados fornecidos e adicionada à lista. Para remover um produto, é realizada uma busca na lista pelo produto com o nome informado e, caso encontrado, é removido. Para listar o estoque, percorremos a lista e exibimos as informações de cada produto.

Espero que este código atenda às suas expectativas!