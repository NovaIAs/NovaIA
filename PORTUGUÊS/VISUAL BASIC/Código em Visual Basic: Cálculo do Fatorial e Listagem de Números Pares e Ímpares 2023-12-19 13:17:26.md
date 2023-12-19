Claro! Aqui está um exemplo de código em Visual Basic bastante complexo:

```vb
Module Program
    Sub Main()
        Dim numero As Integer
        Dim resultado As Integer

        Console.WriteLine("Digite um número:")
        numero = Integer.Parse(Console.ReadLine())

        resultado = Fatorial(numero)

        Console.WriteLine("O fatorial de " & numero & " é: " & resultado)

        Dim listaNumeros As New List(Of Integer)
        Dim soma As Integer = 0

        For i As Integer = 1 To resultado
            listaNumeros.Add(i)
            soma += i
        Next

        Console.WriteLine("A soma dos números de 1 a " & resultado & " é: " & soma)

        Dim numerosPares As New List(Of Integer)

        For Each num As Integer In listaNumeros
            If num Mod 2 = 0 Then
                numerosPares.Add(num)
            End If
        Next

        Console.WriteLine("Os números pares de 1 a " & resultado & " são: ")
        For Each par As Integer In numerosPares
            Console.Write(par & " ")
        Next

        Dim numerosImpares As New List(Of Integer)

        For Each num As Integer In listaNumeros
            If num Mod 2 <> 0 Then
                numerosImpares.Add(num)
            End If
        Next

        Console.WriteLine()
        Console.WriteLine("Os números ímpares de 1 a " & resultado & " são: ")
        For Each impar As Integer In numerosImpares
            Console.Write(impar & " ")
        Next

        Console.ReadLine()
    End Sub

    Function Fatorial(ByVal numero As Integer) As Integer
        If numero = 0 Then
            Return 1
        Else
            Return numero * Fatorial(numero - 1)
        End If
    End Function
End Module
```

Neste código, é solicitado ao usuário que digite um número. Em seguida, é calculado o fatorial desse número e exibido na tela. Em seguida, são geradas duas listas: uma com todos os números de 1 até o fatorial e outra apenas com os números pares. As duas listas são exibidas na tela. Por fim, o programa aguarda que o usuário pressione uma tecla antes de encerrar.

Espero que este exemplo atenda às suas expectativas!