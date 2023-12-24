Claro! Aqui está um código complexo em C# que usa os conceitos de herança, polimorfismo e interfaces para criar uma aplicação de um sistema de gerenciamento de uma biblioteca. O código está em português e possui comentários explicativos para facilitar o entendimento:

```csharp
using System;
using System.Collections.Generic;

// Classe base para representar uma publicação
public abstract class Publicacao
{
    public string Titulo { get; set; }
    public int Ano { get; set; }
    public string Autor { get; set; }

    public virtual void MostrarDetalhes()
    {
        Console.WriteLine($"Título: {Titulo}");
        Console.WriteLine($"Ano: {Ano}");
        Console.WriteLine($"Autor: {Autor}");
    }
}

// Classe para representar um livro
public class Livro : Publicacao
{
    public string Editora { get; set; }

    public override void MostrarDetalhes()
    {
        base.MostrarDetalhes();
        Console.WriteLine($"Editora: {Editora}");
    }
}

// Classe para representar um periódico
public class Periodico : Publicacao
{
    public int NumeroEdicao { get; set; }

    public override void MostrarDetalhes()
    {
        base.MostrarDetalhes();
        Console.WriteLine($"Número da edição: {NumeroEdicao}");
    }
}

// Interface para representar uma biblioteca
public interface IBiblioteca
{
    void AdicionarPublicacao(Publicacao publicacao);
    void MostrarPublicacoes();
}

// Classe para representar uma biblioteca com implementação da interface IBiblioteca
public class Biblioteca : IBiblioteca
{
    private List<Publicacao> _publicacoes;

    public Biblioteca()
    {
        _publicacoes = new List<Publicacao>();
    }

    public void AdicionarPublicacao(Publicacao publicacao)
    {
        _publicacoes.Add(publicacao);
    }

    public void MostrarPublicacoes()
    {
        foreach (var publicacao in _publicacoes)
        {
            publicacao.MostrarDetalhes();
            Console.WriteLine();
        }
    }
}

public class Program
{
    public static void Main(string[] args)
    {
        // Criação de algumas publicações
        Livro livro1 = new Livro
        {
            Titulo = "Aprendendo C#",
            Ano = 2021,
            Autor = "João Silva",
            Editora = "Editora A"
        };

        Periodico periodico1 = new Periodico
        {
            Titulo = "Revista de Programação",
            Ano = 2021,
            Autor = "Maria Souza",
            NumeroEdicao = 1
        };

        Livro livro2 = new Livro
        {
            Titulo = "Estruturas de Dados em C#",
            Ano = 2020,
            Autor = "Pedro Santos",
            Editora = "Editora B"
        };

        // Criação da biblioteca e adição das publicações
        Biblioteca biblioteca = new Biblioteca();
        biblioteca.AdicionarPublicacao(livro1);
        biblioteca.AdicionarPublicacao(periodico1);
        biblioteca.AdicionarPublicacao(livro2);

        // Mostra as publicações da biblioteca
        biblioteca.MostrarPublicacoes();
    }
}
```

Neste código, temos as classes `Publicacao`, `Livro` e `Periodico`, que representam as entidades básicas do sistema de gerenciamento da biblioteca. A classe `Publicacao` é abstrata e define as propriedades comuns a todas as publicações, como título, ano e autor. A classe `Livro` herda de `Publicacao` e possui uma propriedade adicional, `Editora`. A classe `Periodico` também herda de `Publicacao` e possui a propriedade `NumeroEdicao`.

A interface `IBiblioteca` define os métodos `AdicionarPublicacao` e `MostrarPublicacoes`, que devem ser implementados pela classe `Biblioteca`. A classe `Biblioteca` utiliza uma lista para armazenar as publicações adicionadas.

No método `Main`, criamos algumas instâncias de `Livro` e `Periodico`, adicionamos essas publicações à biblioteca e, em seguida, chamamos o método `MostrarPublicacoes` para exibir os detalhes de cada publicação.

Espero que esse código complexo seja útil para você! Se tiver alguma dúvida, fique à vontade para perguntar.