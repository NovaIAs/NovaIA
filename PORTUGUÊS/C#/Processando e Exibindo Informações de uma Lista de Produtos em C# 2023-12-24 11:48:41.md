Claro! Aqui está um código complexo em C# que utiliza várias funcionalidades da linguagem para processar e exibir informações sobre uma lista de produtos:

```csharp
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Criar uma lista de produtos
        List<Produto> produtos = new List<Produto>();

        // Adicionar produtos à lista
        produtos.Add(new Produto("Notebook", 2500.00m, 5));
        produtos.Add(new Produto("Smartphone", 1500.00m, 10));
        produtos.Add(new Produto("Tablet", 800.00m, 3));

        // Exibir informações dos produtos
        Console.WriteLine("Lista de Produtos:");
        Console.WriteLine("=================");
        foreach (Produto produto in produtos)
        {
            produto.ExibirInformacoes();
            Console.WriteLine("-----------------");
        }

        // Calcular e exibir o total de produtos em estoque
        int totalEstoque = CalcularTotalEstoque(produtos);
        Console.WriteLine("Total de Produtos em Estoque: " + totalEstoque);

        // Calcular e exibir o valor total do estoque
        decimal valorTotalEstoque = CalcularValorTotalEstoque(produtos);
        Console.WriteLine("Valor Total do Estoque: R$" + valorTotalEstoque);

        // Esperar pelo pressionamento de uma tecla para finalizar o programa
        Console.ReadKey();
    }

    static int CalcularTotalEstoque(List<Produto> produtos)
    {
        int totalEstoque = 0;
        foreach (Produto produto in produtos)
        {
            totalEstoque += produto.Quantidade;
        }
        return totalEstoque;
    }

    static decimal CalcularValorTotalEstoque(List<Produto> produtos)
    {
        decimal valorTotalEstoque = 0;
        foreach (Produto produto in produtos)
        {
            valorTotalEstoque += produto.Preco * produto.Quantidade;
        }
        return valorTotalEstoque;
    }
}

class Produto
{
    public string Nome { get; private set; }
    public decimal Preco { get; private set; }
    public int Quantidade { get; private set; }

    public Produto(string nome, decimal preco, int quantidade)
    {
        Nome = nome;
        Preco = preco;
        Quantidade = quantidade;
    }

    public void ExibirInformacoes()
    {
        Console.WriteLine("Nome: " + Nome);
        Console.WriteLine("Preço: R$" + Preco);
        Console.WriteLine("Quantidade: " + Quantidade);
    }
}
```

Neste código, criamos uma classe `Produto` com propriedades para armazenar o nome, o preço e a quantidade de cada produto. Também adicionamos um método `ExibirInformacoes()` para exibir as informações de cada produto.

Na função `Main()`, criamos uma lista de produtos e adicionamos alguns produtos a ela. Em seguida, iteramos sobre a lista e chamamos o método `ExibirInformacoes()` para exibir as informações de cada produto.

Depois, temos as funções `CalcularTotalEstoque()` e `CalcularValorTotalEstoque()`, que calculam o total de produtos em estoque e o valor total do estoque, respectivamente. Essas funções iteram sobre a lista de produtos e realizam os cálculos necessários.

Por fim, exibimos o total de produtos em estoque e o valor total do estoque na tela. O programa espera pelo pressionamento de uma tecla antes de finalizar.