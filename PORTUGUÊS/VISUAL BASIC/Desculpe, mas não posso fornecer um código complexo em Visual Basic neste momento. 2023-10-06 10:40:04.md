Claro! Aqui está um código complexo em Visual Basic, que faz uso de várias funcionalidades da linguagem:

```vb
Imports System

Module MainModule

    Sub Main()
        Dim numeros As New List(Of Integer)()

        ' Preenche a lista com números primos de 1 a 100
        For i As Integer = 1 To 100
            If IsPrimo(i) Then
                numeros.Add(i)
            End If
        Next

        ' Ordena a lista em ordem decrescente
        numeros.Sort()
        numeros.Reverse()

        ' Calcula a soma dos números primos
        Dim soma As Integer = 0
        For Each numero As Integer In numeros
            soma += numero
        Next

        ' Imprime o resultado
        Console.WriteLine($"A soma dos números primos de 1 a 100 é: {soma}")

        ' Aguarda a entrada do usuário antes de encerrar o programa
        Console.ReadLine()
    End Sub

    Function IsPrimo(numero As Integer) As Boolean
        ' Verifica se o número é primo
        If numero <= 1 Then
            Return False
        End If

        For i As Integer = 2 To Math.Sqrt(numero)
            If numero Mod i = 0 Then
                Return False
            End If
        Next

        Return True
    End Function

End Module
```

Este código calcula a soma de todos os números primos de 1 a 100 em Visual Basic. 

Primeiro, criamos uma lista chamada "numeros" para armazenar os números primos encontrados. Em seguida, usamos um loop "For" para iterar de 1 a 100 e verificamos se cada número é primo usando a função "IsPrimo". Se o número for primo, ele é adicionado à lista.

Depois, usamos a função "Sort" para ordenar a lista em ordem crescente e, em seguida, usamos a função "Reverse" para inverter a ordem e obter uma lista em ordem decrescente.

Em seguida, usamos outro loop "For Each" para percorrer cada número da lista e somamos esses números à variável "soma".

Por fim, imprimimos o resultado da soma dos números primos e aguardamos a entrada do usuário para encerrar o programa.

A função "IsPrimo" é responsável por verificar se um número é primo. Ela retorna "False" se o número for menor ou igual a 1 ou se for divisível por algum número entre 2 e a raiz quadrada do número (ou seja, se houver algum divisor além de 1 e ele mesmo). Caso contrário, retorna "True".

Este código demonstra o uso de loops, listas, funções, expressões condicionais e matemática básica em Visual Basic.