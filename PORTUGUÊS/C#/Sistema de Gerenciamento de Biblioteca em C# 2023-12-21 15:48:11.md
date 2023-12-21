Claro! Aqui está um exemplo de um código complexo em C# que implementa um sistema de gerenciamento de biblioteca:

```csharp
using System;
using System.Collections.Generic;

namespace SistemaBiblioteca
{
    class Program
    {
        static void Main(string[] args)
        {
            Biblioteca biblioteca = new Biblioteca();

            // Adiciona alguns livros à biblioteca
            biblioteca.AdicionarLivro(new Livro("Harry Potter e a Pedra Filosofal", "J.K. Rowling", 1997));
            biblioteca.AdicionarLivro(new Livro("O Senhor dos Anéis: A Sociedade do Anel", "J.R.R. Tolkien", 1954));
            biblioteca.AdicionarLivro(new Livro("Orgulho e Preconceito", "Jane Austen", 1813));

            bool executando = true;
            while (executando)
            {
                Console.Clear();
                Console.WriteLine("===== Sistema de Gerenciamento de Biblioteca =====");
                Console.WriteLine("1. Listar todos os livros");
                Console.WriteLine("2. Adicionar novo livro");
                Console.WriteLine("3. Remover livro");
                Console.WriteLine("4. Sair");
                Console.Write("Escolha uma opção: ");

                int opcao;
                if (int.TryParse(Console.ReadLine(), out opcao))
                {
                    switch (opcao)
                    {
                        case 1:
                            Console.Clear();
                            Console.WriteLine("===== Lista de Livros =====");
                            biblioteca.ListarLivros();
                            Console.WriteLine("===========================");
                            Console.WriteLine("Pressione qualquer tecla para continuar...");
                            Console.ReadKey();
                            break;
                        case 2:
                            Console.Clear();
                            Console.WriteLine("===== Adicionar Novo Livro =====");
                            Console.Write("Digite o título do livro: ");
                            string titulo = Console.ReadLine();
                            Console.Write("Digite o autor do livro: ");
                            string autor = Console.ReadLine();
                            Console.Write("Digite o ano de lançamento do livro: ");
                            int ano;
                            if (int.TryParse(Console.ReadLine(), out ano))
                            {
                                biblioteca.AdicionarLivro(new Livro(titulo, autor, ano));
                                Console.WriteLine("Livro adicionado com sucesso!");
                            }
                            else
                            {
                                Console.WriteLine("Ano inválido! O livro não foi adicionado.");
                            }
                            Console.WriteLine("Pressione qualquer tecla para continuar...");
                            Console.ReadKey();
                            break;
                        case 3:
                            Console.Clear();
                            Console.WriteLine("===== Remover Livro =====");
                            Console.Write("Digite o título do livro que deseja remover: ");
                            string tituloRemover = Console.ReadLine();
                            bool removido = biblioteca.RemoverLivro(tituloRemover);
                            if (removido)
                            {
                                Console.WriteLine("Livro removido com sucesso!");
                            }
                            else
                            {
                                Console.WriteLine("Livro não encontrado. Nenhum livro foi removido.");
                            }
                            Console.WriteLine("Pressione qualquer tecla para continuar...");
                            Console.ReadKey();
                            break;
                        case 4:
                            executando = false;
                            break;
                        default:
                            Console.WriteLine("Opção inválida! Tente novamente.");
                            Console.WriteLine("Pressione qualquer tecla para continuar...");
                            Console.ReadKey();
                            break;
                    }
                }
                else
                {
                    Console.WriteLine("Opção inválida! Tente novamente.");
                    Console.WriteLine("Pressione qualquer tecla para continuar...");
                    Console.ReadKey();
                }
            }
        }
    }

    class Livro
    {
        public string Titulo { get; set; }
        public string Autor { get; set; }
        public int AnoLancamento { get; set; }

        public Livro(string titulo, string autor, int anoLancamento)
        {
            Titulo = titulo;
            Autor = autor;
            AnoLancamento = anoLancamento;
        }

        public override string ToString()
        {
            return $"Título: {Titulo}\tAutor: {Autor}\tAno de lançamento: {AnoLancamento}";
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

        public bool RemoverLivro(string titulo)
        {
            Livro livro = livros.Find(l => l.Titulo.Equals(titulo));
            if (livro != null)
            {
                livros.Remove(livro);
                return true;
            }
            return false;
        }

        public void ListarLivros()
        {
            foreach (Livro livro in livros)
            {
                Console.WriteLine(livro);
            }
        }
    }
}
```

Este código implementa um sistema de gerenciamento de biblioteca, onde os usuários podem listar todos os livros disponíveis, adicionar novos livros e remover livros existentes. O código utiliza classes para representar os livros e a biblioteca, e também faz uso de uma lista para armazenar os livros adicionados.

A função principal (`Main`) exibe um menu de opções para o usuário e executa a ação correspondente à opção selecionada. As opções incluem listar todos os livros, adicionar um novo livro, remover um livro e sair do programa.

O código também inclui validações para garantir que as entradas do usuário sejam corretas, como checar se o ano de lançamento é um número válido. Caso uma entrada inválida seja fornecida, uma mensagem de erro é exibida.

Espero que esse código atenda às suas necessidades!