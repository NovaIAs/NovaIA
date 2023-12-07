Claro! Aqui está um código complexo em Visual Basic, que atende à sua solicitação:

```vb
Imports System

Public Class Program
    Public Shared Sub Main()
        ' Definir uma função recursiva para encontrar o fatorial de um número
        Console.WriteLine("Digite um número inteiro positivo:")
        Dim numero As Integer = Convert.ToInt32(Console.ReadLine())
        Dim fatorial As Integer = EncontrarFatorial(numero)
        Console.WriteLine("O fatorial de {0} é: {1}", numero, fatorial)
        
        ' Definir uma função para verificar se uma palavra é um palíndromo
        Console.WriteLine("Digite uma palavra:")
        Dim palavra As String = Console.ReadLine()
        If VerificarPalindromo(palavra) Then
            Console.WriteLine("{0} é um palíndromo!", palavra)
        Else
            Console.WriteLine("{0} não é um palíndromo!", palavra)
        End If
        
        ' Definir uma classe para representar um retângulo
        Dim retangulo As New Retangulo(5, 3)
        Console.WriteLine("Retângulo:")
        Console.WriteLine("Largura: {0}", retangulo.Largura)
        Console.WriteLine("Altura: {0}", retangulo.Altura)
        Console.WriteLine("Área: {0}", retangulo.CalcularArea())
        Console.WriteLine("Perímetro: {0}", retangulo.CalcularPerimetro())
        
        Console.ReadLine()
    End Sub
    
    Private Shared Function EncontrarFatorial(numero As Integer) As Integer
        If numero = 0 Or numero = 1 Then
            Return 1
        Else
            Return numero * EncontrarFatorial(numero - 1)
        End If
    End Function
    
    Private Shared Function VerificarPalindromo(palavra As String) As Boolean
        palavra = palavra.ToLower()
        Dim palindromo As Boolean = True
        Dim inicio As Integer = 0
        Dim fim As Integer = palavra.Length - 1
        
        While inicio < fim
            If palavra(inicio) <> palavra(fim) Then
                palindromo = False
                Exit While
            End If
            inicio += 1
            fim -= 1
        End While

        Return palindromo
    End Function
    
    Private Class Retangulo
        Public Property Largura As Integer
        Public Property Altura As Integer
        
        Public Sub New(largura As Integer, altura As Integer)
            Me.Largura = largura
            Me.Altura = altura
        End Sub
        
        Public Function CalcularArea() As Integer
            Return Largura * Altura
        End Function
        
        Public Function CalcularPerimetro() As Integer
            Return 2 * (Largura + Altura)
        End Function
    End Class
End Class

```
Neste código, primeiro definimos uma função recursiva chamada `EncontrarFatorial` para encontrar o fatorial de um número fornecido pelo usuário. Em seguida, temos uma função chamada `VerificarPalindromo` que verifica se uma palavra fornecida pelo usuário é um palíndromo.

Em seguida, criamos uma classe chamada `Retangulo` para representar um retângulo e calculamos sua área e perímetro. No `Sub Main`, solicitamos ao usuário um número para encontrar seu fatorial, uma palavra para verificar se é um palíndromo e, finalmente, criamos uma instância de um retângulo e exibimos suas propriedades.

Espero que você ache este código complexo o suficiente e que se adeque às suas necessidades!