Claro! Aqui está um código complexo em Visual Basic, com comentários explicando cada parte:

```vb
Imports System

Module MainModule
    Sub Main()
        Console.WriteLine("Bem-vindo ao programa complexo!")

        ' Variáveis
        Dim contador As Integer = 0
        Dim soma As Double = 0

        ' Loop enquanto o contador for menor que 10
        While contador < 10
            ' Gera um número aleatório entre 1 e 100
            Dim numeroAleatorio As Integer = New Random().Next(1, 101)
            
            ' Verifica se o número gerado é par
            If numeroAleatorio Mod 2 = 0 Then
                ' Se for par, soma ao valor atual da variável soma
                soma += numeroAleatorio
            End If

            ' Imprime o número gerado
            Console.WriteLine("Número gerado: " & numeroAleatorio)

            ' Incrementa o contador
            contador += 1
        End While

        ' Imprime a soma dos números pares gerados
        Console.WriteLine("A soma dos números pares gerados é: " & soma)

        ' Aguarda o pressionamento de uma tecla para encerrar o programa
        Console.ReadKey()
    End Sub
End Module
```

Neste código, criamos um programa que gera 10 números aleatórios entre 1 e 100 e verifica se cada número gerado é par. Se for par, ele é somado a uma variável chamada "soma". No final, o programa imprime a soma dos números pares gerados.

Espero que este código atenda às suas expectativas! Se tiver mais alguma solicitação, estou à disposição.