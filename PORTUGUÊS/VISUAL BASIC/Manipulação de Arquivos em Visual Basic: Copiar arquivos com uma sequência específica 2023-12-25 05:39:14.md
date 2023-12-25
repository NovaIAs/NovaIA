Claro! Aqui está um código complexo em Visual Basic que realiza uma tarefa de manipulação de arquivos. O código foi desenvolvido para realizar a seguinte funcionalidade: dado um diretório de origem, o código percorrerá recursivamente todos os arquivos e subdiretórios dentro dele, verificando se o nome de algum arquivo contém uma determinada sequência de caracteres. Se encontrar um arquivo com a sequência, o código fará uma cópia do arquivo para um diretório de destino especificado.

```vb
Imports System.IO

Module Module1
    Sub Main()
        Dim diretorioOrigem As String = "C:\DiretorioOrigem"
        Dim diretorioDestino As String = "C:\DiretorioDestino"
        Dim sequencia As String = "exemplo"

        CopiarArquivos(diretorioOrigem, diretorioDestino, sequencia)

        Console.WriteLine("Operação concluída. Pressione qualquer tecla para sair.")
        Console.ReadKey()
    End Sub

    Sub CopiarArquivos(ByVal diretorioOrigem As String, ByVal diretorioDestino As String, ByVal sequencia As String)
        If Not Directory.Exists(diretorioOrigem) Then
            Console.WriteLine("Diretório de origem não existe.")
            Return
        End If

        If Not Directory.Exists(diretorioDestino) Then
            Directory.CreateDirectory(diretorioDestino)
        End If

        Dim arquivos As String() = Directory.GetFiles(diretorioOrigem)
        Dim subdiretorios As String() = Directory.GetDirectories(diretorioOrigem)

        For Each arquivo In arquivos
            If Path.GetFileName(arquivo).Contains(sequencia) Then
                Dim nomeArquivoDestino As String = Path.Combine(diretorioDestino, Path.GetFileName(arquivo))
                File.Copy(arquivo, nomeArquivoDestino, True)
                Console.WriteLine("Arquivo copiado: " & nomeArquivoDestino)
            End If
        Next

        For Each subdiretorio In subdiretorios
            Dim novoDiretorioDestino As String = Path.Combine(diretorioDestino, Path.GetFileName(subdiretorio))
            CopiarArquivos(subdiretorio, novoDiretorioDestino, sequencia)
        Next
    End Sub
End Module
```

Explicação do código:

1. Primeiro, importamos o namespace `System.IO`, que contém as classes para manipulação de arquivos e diretórios.

2. Em seguida, definimos o módulo `Module1` e o método `Main` como ponto de entrada do programa.

3. Dentro do método `Main`, definimos as variáveis `diretorioOrigem`, `diretorioDestino` e `sequencia`. O `diretorioOrigem` é o diretório de origem onde os arquivos serão procurados, `diretorioDestino` é o diretório onde os arquivos encontrados serão copiados e `sequencia` é a sequência de caracteres que queremos procurar nos nomes dos arquivos.

4. Chamamos o método `CopiarArquivos`, passando as variáveis `diretorioOrigem`, `diretorioDestino` e `sequencia` como argumentos.

5. No método `CopiarArquivos`, primeiramente verificamos se o diretório de origem existe. Se não existir, exibimos uma mensagem de erro e retornamos.

6. Em seguida, verificamos se o diretório de destino existe. Se não existir, criamos o diretório.

7. Utilizamos o método `Directory.GetFiles` para obter uma lista de arquivos no diretório de origem.

8. Utilizamos o método `Directory.GetDirectories` para obter uma lista de subdiretórios no diretório de origem.

9. Percorremos os arquivos encontrados utilizando um loop `For Each`.

10. Dentro do loop, verificamos se o nome do arquivo contém a sequência de caracteres utilizando o método `Path.GetFileName` e o método `String.Contains`. Se contiver, copiamos o arquivo para o diretório de destino utilizando o método `File.Copy`, passando o caminho do arquivo original, o caminho do arquivo de destino e o argumento `True` para indicar que permitimos a substituição de arquivos existentes.

11. Exibimos uma mensagem informando que o arquivo foi copiado.

12. Após o loop de arquivos, percorremos os subdiretórios utilizando um loop `For Each`.

13. Dentro do loop, criamos um novo diretório de destino combinando o nome do subdiretório com o diretório de destino atual utilizando o método `Path.Combine`.

14. Chamamos recursivamente o método `CopiarArquivos`, passando o subdiretório como novo diretório de origem e o novo diretório de destino.

15. Ao final do processo, exibimos uma mensagem informando que a operação foi concluída e aguardamos a pressionamento de qualquer tecla para sair.

Espero que este código atenda às suas necessidades!