Claro! Aqui está um código complexo em C# que cria uma aplicação de gerenciamento de estoque usando uma abordagem orientada a objetos.

```csharp
using System;
using System.Collections.Generic;

// Classe Produto representa cada item no estoque
public class Produto
{
    public int Id { get; set; }
    public string Nome { get; set; }
    public int Quantidade { get; set; }
    public decimal Preco { get; set; }

    // Construtor da classe Produto
    public Produto(int id, string nome, int quantidade, decimal preco)
    {
        Id = id;
        Nome = nome;
        Quantidade = quantidade;
        Preco = preco;
    }

    // Método para exibir as informações do produto
    public void ExibirInformacoes()
    {
        Console.WriteLine($"ID: {Id}");
        Console.WriteLine($"Nome: {Nome}");
        Console.WriteLine($"Quantidade: {Quantidade}");
        Console.WriteLine($"Preço: R${Preco}");
        Console.WriteLine();
    }
}

// Classe Estoque representa o conjunto de produtos
public class Estoque
{
    private List<Produto> produtos;

    // Construtor da classe Estoque
    public Estoque()
    {
        produtos = new List<Produto>();
    }

    // Método para adicionar um novo produto ao estoque
    public void AdicionarProduto(Produto produto)
    {
        produtos.Add(produto);
    }

    // Método para exibir todos os produtos no estoque
    public void ExibirEstoque()
    {
        foreach (var produto in produtos)
        {
            produto.ExibirInformacoes();
        }
    }

    // Método para buscar um produto pelo ID
    public Produto BuscarProdutoPorId(int id)
    {
        foreach (var produto in produtos)
        {
            if (produto.Id == id)
            {
                return produto;
            }
        }

        return null;
    }

    // Método para remover um produto do estoque pelo ID
    public void RemoverProduto(int id)
    {
        var produto = BuscarProdutoPorId(id);

        if (produto != null)
        {
            produtos.Remove(produto);
            Console.WriteLine("Produto removido com sucesso!");
        }
        else
        {
            Console.WriteLine("Produto não encontrado no estoque.");
        }
    }
}

// Classe principal que contém o método Main
public class Program
{
    public static void Main(string[] args)
    {
        // Criação do estoque
        var estoque = new Estoque();

        // Adição de produtos ao estoque
        estoque.AdicionarProduto(new Produto(1, "Camiseta", 10, 29.99m));
        estoque.AdicionarProduto(new Produto(2, "Calça", 5, 59.99m));
        estoque.AdicionarProduto(new Produto(3, "Tênis", 3, 99.99m));

        // Exibição do estoque
        Console.WriteLine("Estoque:");
        estoque.ExibirEstoque();

        // Busca de um produto pelo ID
        Console.WriteLine("Buscando produto pelo ID...");
        var produtoBuscado = estoque.BuscarProdutoPorId(2);
        if (produtoBuscado != null)
        {
            Console.WriteLine("Produto encontrado:");
            produtoBuscado.ExibirInformacoes();
        }
        else
        {
            Console.WriteLine("Produto não encontrado.");
        }

        // Remoção de um produto pelo ID
        Console.WriteLine("Removendo produto pelo ID...");
        estoque.RemoverProduto(1);

        // Exibição atualizada do estoque
        Console.WriteLine("Estoque atualizado:");
        estoque.ExibirEstoque();

        Console.ReadLine();
    }
}
```

Este código cria uma aplicação de gerenciamento de estoque. A classe `Produto` representa cada item no estoque, com propriedades como `Id`, `Nome`, `Quantidade` e `Preco`. A classe `Estoque` é responsável por armazenar e gerenciar os produtos. Ela possui métodos para adicionar, exibir, buscar e remover produtos do estoque.

No método `Main`, um objeto `Estoque` é criado e alguns produtos são adicionados a ele. Em seguida, o estoque é exibido, um produto é buscado pelo ID e removido pelo ID. Por fim, o estoque atualizado é exibido novamente.

Espero que esse código atenda às suas expectativas. Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar!