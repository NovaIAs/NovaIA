Claro! Aqui está um código em C# bastante complexo e diferenciado, que envolve a criação de uma aplicação para gerenciar uma biblioteca virtual. O código está bem documentado para facilitar o entendimento:

```csharp
using System;
using System.Collections.Generic;

namespace BibliotecaVirtual
{
    class Program
    {
        static void Main(string[] args)
        {
            Biblioteca biblioteca = new Biblioteca();

            bool executando = true;
            while (executando)
            {
                Console.Clear();
                Console.WriteLine("===== BIBLIOTECA VIRTUAL =====");
                Console.WriteLine("1 - Adicionar Livro");
                Console.WriteLine("2 - Remover Livro");
                Console.WriteLine("3 - Listar Livros");
                Console.WriteLine("4 - Sair");
                Console.WriteLine("==============================");

                Console.Write("Escolha uma opção: ");
                int opcao = int.Parse(Console.ReadLine());

                switch (opcao)
                {
                    case 1:
                        Console.Clear();
                        Console.WriteLine("===== ADICIONAR LIVRO =====");

                        Console.Write("Título: ");
                        string titulo = Console.ReadLine();

                        Console.Write("Autor: ");
                        string autor = Console.ReadLine();

                        Console.Write("Ano de Publicação: ");
                        int anoPublicacao = int.Parse(Console.ReadLine());

                        Console.Write("ISBN: ");
                        string isbn = Console.ReadLine();

                        Livro novoLivro = new Livro(titulo, autor, anoPublicacao, isbn);
                        biblioteca.AdicionarLivro(novoLivro);

                        Console.WriteLine("\nLivro adicionado com sucesso!");
                        Console.WriteLine("Pressione qualquer tecla para continuar...");
                        Console.ReadKey();
                        break;

                    case 2:
                        Console.Clear();
                        Console.WriteLine("===== REMOVER LIVRO =====");

                        Console.Write("ISBN do livro que deseja remover: ");
                        string isbnRemover = Console.ReadLine();

                        bool livroRemovido = biblioteca.RemoverLivro(isbnRemover);

                        if (livroRemovido)
                        {
                            Console.WriteLine("\nLivro removido com sucesso!");
                        }
                        else
                        {
                            Console.WriteLine("\nLivro não encontrado na biblioteca!");
                        }

                        Console.WriteLine("Pressione qualquer tecla para continuar...");
                        Console.ReadKey();
                        break;

                    case 3:
                        Console.Clear();
                        Console.WriteLine("===== LISTAR LIVROS =====");

                        List<Livro> livros = biblioteca.ListarLivros();

                        if (livros.Count == 0)
                        {
                            Console.WriteLine("\nNão há livros cadastrados na biblioteca!");
                        }
                        else
                        {
                            Console.WriteLine("\nLivros na biblioteca:\n");

                            foreach (Livro livro in livros)
                            {
                                Console.WriteLine(livro);
                                Console.WriteLine("-----------------------------");
                            }
                        }

                        Console.WriteLine("\nPressione qualquer tecla para continuar...");
                        Console.ReadKey();
                        break;

                    case 4:
                        Console.Clear();
                        Console.WriteLine("Saindo da aplicação...");
                        executando = false;
                        break;

                    default:
                        Console.Clear();
                        Console.WriteLine("Opção inválida! Tente novamente.");
                        Console.WriteLine("Pressione qualquer tecla para continuar...");
                        Console.ReadKey();
                        break;
                }
            }
        }
    }

    class Livro
    {
        public string Titulo { get; }
        public string Autor { get; }
        public int AnoPublicacao { get; }
        public string ISBN { get; }

        public Livro(string titulo, string autor, int anoPublicacao, string isbn)
        {
            Titulo = titulo;
            Autor = autor;
            AnoPublicacao = anoPublicacao;
            ISBN = isbn;
        }

        public override string ToString()
        {
            return $"Título: {Titulo}\nAutor: {Autor}\nAno de Publicação: {AnoPublicacao}\nISBN: {ISBN}";
        }
    }

    class Biblioteca
    {
        private List<Livro> livros;

        public Biblioteca()
        {
            livros = new List<Livro>();
        }

        public void AdicionarLivro(Livro livro)
        {
            livros.Add(livro);
        }

        public bool RemoverLivro(string isbn)
        {
            Livro livroRemover = livros.Find(livro => livro.ISBN == isbn);
            
            if (livroRemover != null)
            {
                livros.Remove(livroRemover);
                return true;
            }

            return false;
        }

        public List<Livro> ListarLivros()
        {
            return livros;
        }
    }
}
```

Neste código, criamos três classes: `Livro`, `Biblioteca`, e a classe principal `Program`. A classe `Livro` representa um livro com seus atributos como título, autor, ano de publicação e ISBN. A classe `Biblioteca` é responsável por gerenciar uma lista de livros, permitindo adicionar, remover e listar os livros. A classe `Program` é a classe principal que contém o loop de execução do menu da biblioteca.

A aplicação permite ao usuário adicionar livros à biblioteca, informando o título, autor, ano de publicação e ISBN. Também é possível remover livros da biblioteca informando o ISBN do livro desejado. Além disso, é possível listar todos os livros cadastrados na biblioteca.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, é só me dizer.