Claro! Aqui está um exemplo de um código complexo em Visual Basic que envolve manipulação de arquivos, estruturas de controle e funções:

```
Imports System.IO

Module MainModule

    Sub Main()
        Dim diretorio As String = "C:\Exemplo"
        Dim arquivos As String() = Directory.GetFiles(diretorio)

        Console.WriteLine("Bem-vindo ao programa de manipulação de arquivos!")
        Console.WriteLine("Escolha uma opção:")
        Console.WriteLine("1 - Listar arquivos")
        Console.WriteLine("2 - Copiar arquivos")
        Console.WriteLine("3 - Mover arquivos")
        Console.WriteLine("4 - Renomear arquivo")
        Console.WriteLine("5 - Sair")

        Dim opcao As Integer = Console.ReadLine()

        Select Case opcao
            Case 1
                ListarArquivos(arquivos)
            Case 2
                Console.WriteLine("Digite o diretório de destino:")
                Dim diretorioDestino As String = Console.ReadLine()
                CopiarArquivos(arquivos, diretorioDestino)
            Case 3
                Console.WriteLine("Digite o diretório de destino:")
                Dim diretorioDestino As String = Console.ReadLine()
                MoverArquivos(arquivos, diretorioDestino)
            Case 4
                Console.WriteLine("Digite o novo nome do arquivo:")
                Dim novoNome As String = Console.ReadLine()
                RenomearArquivo(arquivos(0), novoNome)
            Case 5
                Exit Sub
            Case Else
                Console.WriteLine("Opção inválida!")
        End Select
    End Sub

    Sub ListarArquivos(ByVal arquivos As String())
        Console.WriteLine("Arquivos encontrados:")
        For Each arquivo In arquivos
            Console.WriteLine(arquivo)
        Next
    End Sub

    Sub CopiarArquivos(ByVal arquivos As String(), ByVal diretorioDestino As String)
        If Not Directory.Exists(diretorioDestino) Then
            Directory.CreateDirectory(diretorioDestino)
        End If

        For Each arquivo In arquivos
            Dim nomeArquivo As String = Path.GetFileName(arquivo)
            Dim caminhoDestino As String = Path.Combine(diretorioDestino, nomeArquivo)
            File.Copy(arquivo, caminhoDestino, True)
        Next

        Console.WriteLine("Arquivos copiados com sucesso!")
    End Sub

    Sub MoverArquivos(ByVal arquivos As String(), ByVal diretorioDestino As String)
        If Not Directory.Exists(diretorioDestino) Then
            Directory.CreateDirectory(diretorioDestino)
        End If

        For Each arquivo In arquivos
            Dim nomeArquivo As String = Path.GetFileName(arquivo)
            Dim caminhoDestino As String = Path.Combine(diretorioDestino, nomeArquivo)
            File.Move(arquivo, caminhoDestino)
        Next

        Console.WriteLine("Arquivos movidos com sucesso!")
    End Sub

    Sub RenomearArquivo(ByVal arquivo As String, ByVal novoNome As String)
        Dim diretorioArquivo As String = Path.GetDirectoryName(arquivo)
        Dim novoCaminho As String = Path.Combine(diretorioArquivo, novoNome)
        File.Move(arquivo, novoCaminho)

        Console.WriteLine("Arquivo renomeado com sucesso!")
    End Sub

End Module
```

Neste código, há várias funcionalidades, como listar arquivos em um diretório, copiar arquivos para outro diretório, mover arquivos para outro diretório e renomear um arquivo. A estrutura de controle `Select Case` é utilizada para permitir ao usuário escolher a opção desejada. As funções `ListarArquivos`, `CopiarArquivos`, `MoverArquivos` e `RenomearArquivo` são responsáveis por realizar as operações correspondentes, utilizando classes do namespace `System.IO` para manipulação de arquivos e diretórios.

Espero que este código atenda às suas expectativas!