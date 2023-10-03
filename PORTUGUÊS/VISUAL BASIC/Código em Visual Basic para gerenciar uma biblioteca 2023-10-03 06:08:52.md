Claro! Aqui está um código complexo em Visual Basic que cria um programa para gerenciar uma biblioteca:

```vb
Imports System

Module Biblioteca
    ' Classe para representar um livro na biblioteca
    Class Livro
        Public Property Titulo As String
        Public Property Autor As String
        Public Property AnoPublicacao As Integer
        Public Property Disponivel As Boolean

        Public Sub New(titulo As String, autor As String, anoPublicacao As Integer)
            Me.Titulo = titulo
            Me.Autor = autor
            Me.AnoPublicacao = anoPublicacao
            Me.Disponivel = True
        End Sub
    End Class

    ' Classe para representar a biblioteca
    Class Biblioteca
        Private Property Livros As List(Of Livro)

        Public Sub New()
            Me.Livros = New List(Of Livro)()
        End Sub

        Public Sub AdicionarLivro(livro As Livro)
            Me.Livros.Add(livro)
        End Sub

        Public Sub RemoverLivro(livro As Livro)
            Me.Livros.Remove(livro)
        End Sub

        Public Function PesquisarLivro(titulo As String) As Livro
            For Each livro As Livro In Me.Livros
                If livro.Titulo.Equals(titulo, StringComparison.OrdinalIgnoreCase) Then
                    Return livro
                End If
            Next
            Return Nothing
        End Function

        Public Sub ListarLivrosDisponiveis()
            For Each livro As Livro In Me.Livros
                If livro.Disponivel Then
                    Console.WriteLine("Título: " & livro.Titulo)
                    Console.WriteLine("Autor: " & livro.Autor)
                    Console.WriteLine("Ano de Publicação: " & livro.AnoPublicacao)
                    Console.WriteLine()
                End If
            Next
        End Sub
    End Class

    Sub Main()
        Dim biblioteca As New Biblioteca()

        ' Adicionando alguns livros à biblioteca
        biblioteca.AdicionarLivro(New Livro("Dom Quixote", "Miguel de Cervantes", 1605))
        biblioteca.AdicionarLivro(New Livro("1984", "George Orwell", 1949))
        biblioteca.AdicionarLivro(New Livro("O Pequeno Príncipe", "Antoine de Saint-Exupéry", 1943))

        ' Pesquisando um livro
        Dim livroPesquisado As Livro = biblioteca.PesquisarLivro("1984")
        If livroPesquisado IsNot Nothing Then
            Console.WriteLine("Livro encontrado:")
            Console.WriteLine("Título: " & livroPesquisado.Titulo)
            Console.WriteLine("Autor: " & livroPesquisado.Autor)
            Console.WriteLine("Ano de Publicação: " & livroPesquisado.AnoPublicacao)
            Console.WriteLine()
        Else
            Console.WriteLine("Livro não encontrado.")
        End If

        ' Listando os livros disponíveis
        Console.WriteLine("Livros disponíveis:")
        biblioteca.ListarLivrosDisponiveis()

        Console.ReadLine()
    End Sub
End Module
```

Neste código, temos duas classes principais: `Livro` e `Biblioteca`. A classe `Livro` representa cada livro na biblioteca, com propriedades como `Titulo`, `Autor`, `AnoPublicacao` e `Disponivel`. A classe `Biblioteca` representa a própria biblioteca, com métodos para adicionar e remover livros, pesquisar livros por título e listar os livros disponíveis.

No procedimento `Main`, criamos uma instância da classe `Biblioteca`, adicionamos alguns livros à biblioteca e realizamos uma pesquisa por um livro específico. Por fim, listamos os livros disponíveis.

Este código demonstra o uso de classes, propriedades, métodos, loops e estruturas condicionais em Visual Basic, bem como a organização das classes em um módulo.