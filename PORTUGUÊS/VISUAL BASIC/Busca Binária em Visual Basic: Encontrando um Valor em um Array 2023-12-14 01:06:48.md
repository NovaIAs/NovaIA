Claro! Aqui está um código em Visual Basic que realiza uma busca binária em um array de números inteiros:

```vb
Imports System

Module Program
    Sub Main(args As String())
        Dim numeros() As Integer = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
        Dim valorProcurado As Integer = 8

        Dim resultado As Integer = BuscaBinaria(numeros, valorProcurado)

        Console.WriteLine("O valor {0} está na posição {1}.", valorProcurado, resultado)
        Console.ReadLine()
    End Sub

    Function BuscaBinaria(arr() As Integer, valor As Integer) As Integer
        Dim inicio As Integer = 0
        Dim fim As Integer = arr.Length - 1

        While inicio <= fim
            Dim meio As Integer = (inicio + fim) \ 2

            If arr(meio) = valor Then
                Return meio
            ElseIf arr(meio) < valor Then
                inicio = meio + 1
            Else
                fim = meio - 1
            End If
        End While

        Return -1
    End Function
End Module
```

Neste código, temos um array chamado "numeros" que contém os números de 1 a 10. O valor que estamos procurando é o 8. A função `BuscaBinaria` implementa o algoritmo de busca binária para encontrar o valor no array.

A busca binária começa definindo um índice de início e um índice de fim. Em cada iteração do loop while, o código calcula o índice do elemento do meio do array e compara o valor nessa posição com o valor procurado. Se forem iguais, a função retorna o índice do meio. Se o valor do meio for menor que o valor procurado, o índice de início é atualizado para o meio + 1. Caso contrário, o índice de fim é atualizado para o meio - 1.

Esse processo é repetido até que o valor seja encontrado ou até que o índice de início seja maior que o índice de fim, indicando que o valor não está presente no array. No final, a função retorna -1 para indicar que o valor não foi encontrado.

No código principal, chamamos a função `BuscaBinaria` passando o array `numeros` e o valor `valorProcurado`. O resultado da busca é armazenado na variável `resultado` e, em seguida, exibido no console.

Este código demonstra uma implementação simples e eficiente de busca binária em Visual Basic para encontrar um valor específico em um array de números inteiros.