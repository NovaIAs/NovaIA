Claro! Aqui está um exemplo de código em Visual Basic que realiza a ordenação de um vetor utilizando o algoritmo de ordenação por inserção:

```
Module MainModule
    Sub Main()
        Dim vetor() As Integer = {9, 4, 7, 2, 1, 8, 5, 6, 3}

        Console.WriteLine("Vetor original:")
        ImprimirVetor(vetor)

        Console.WriteLine("Ordenando o vetor...")
        OrdenarVetorPorInsercao(vetor)

        Console.WriteLine("Vetor ordenado:")
        ImprimirVetor(vetor)

        Console.ReadLine()
    End Sub

    Sub ImprimirVetor(ByVal vetor() As Integer)
        For Each elemento As Integer In vetor
            Console.Write(elemento & " ")
        Next
        Console.WriteLine()
    End Sub

    Sub OrdenarVetorPorInsercao(ByRef vetor() As Integer)
        Dim tamanho As Integer = vetor.Length

        For i As Integer = 1 To tamanho - 1
            Dim chave As Integer = vetor(i)
            Dim j As Integer = i - 1

            While j >= 0 AndAlso vetor(j) > chave
                vetor(j + 1) = vetor(j)
                j -= 1
            End While

            vetor(j + 1) = chave
        Next
    End Sub
End Module
```

Neste código, primeiro definimos um vetor de números inteiros desordenados. Em seguida, exibimos o vetor original na tela.

Depois disso, chamamos a função `OrdenarVetorPorInsercao`, que implementa o algoritmo de ordenação por inserção. Esse algoritmo percorre o vetor, movendo os elementos maiores para a direita, até encontrar a posição correta para inserir cada elemento.

Em seguida, exibimos o vetor ordenado na tela.

A função `ImprimirVetor` é responsável por exibir o vetor na tela, separando os elementos por espaço.

Espero que este exemplo atenda às suas expectativas!