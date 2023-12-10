Certamente, vou criar um código complexo em Visual Basic e explicar passo a passo o que cada parte faz. Aqui está o código:

```visualbasic
Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.Threading.Tasks

Module MainModule

    Sub Main()
        Console.WriteLine("Bem-vindo ao meu programa em Visual Basic!")
        Console.WriteLine("Digite um número para iniciar:")
        Dim numero As Integer = Convert.ToInt32(Console.ReadLine())

        Dim resultado As Integer = CalcularFatorial(numero)
        Console.WriteLine("O fatorial de " & numero & " é: " & resultado)

        Dim listaPrimos As List(Of Integer) = GerarListaPrimos(numero)
        Console.WriteLine("Os números primos até " & numero & " são:")
        For Each primo In listaPrimos
            Console.WriteLine(primo)
        Next

        Dim somaDivisores As Integer = CalcularSomaDivisores(numero)
        Console.WriteLine("A soma de todos os divisores de " & numero & " é: " & somaDivisores)

        Console.WriteLine("Pressione qualquer tecla para sair...")
        Console.ReadKey()
    End Sub

    Function CalcularFatorial(ByVal n As Integer) As Integer
        Dim fatorial As Integer = 1
        For i As Integer = 1 To n
            fatorial *= i
        Next
        Return fatorial
    End Function

    Function GerarListaPrimos(ByVal n As Integer) As List(Of Integer)
        Dim lista As New List(Of Integer)
        For i As Integer = 2 To n
            Dim isPrimo As Boolean = True
            For j As Integer = 2 To Math.Sqrt(i)
                If i Mod j = 0 Then
                    isPrimo = False
                    Exit For
                End If
            Next
            If isPrimo Then
                lista.Add(i)
            End If
        Next
        Return lista
    End Function

    Function CalcularSomaDivisores(ByVal n As Integer) As Integer
        Dim soma As Integer = 0
        For i As Integer = 1 To n
            If n Mod i = 0 Then
                soma += i
            End If
        Next
        Return soma
    End Function

End Module
```

Explicação do código:

1. Importações: 
   - São importadas as bibliotecas necessárias para o código, que incluem `System`, `System.Collections.Generic`, `System.Linq`, `System.Text` e `System.Threading.Tasks`.

2. Módulo principal (`MainModule`):
   - Aqui é onde o programa começa a ser executado.
   - É exibida uma mensagem de boas-vindas e solicita ao usuário que digite um número.
   - O número digitado é convertido para um tipo `Integer` e armazenado na variável `numero`.

3. Função `CalcularFatorial`:
   - Essa função recebe um número inteiro e retorna o fatorial desse número.
   - A variável `fatorial` é inicializada com 1.
   - É feito um loop de 1 até o número digitado pelo usuário, multiplicando o valor da variável `fatorial` pelo valor do loop em cada iteração.
   - O valor final de `fatorial` é retornado.

4. Função `GerarListaPrimos`:
   - Essa função recebe um número inteiro e retorna uma lista de todos os números primos menores ou iguais a esse número.
   - É criada uma lista vazia para armazenar os números primos encontrados.
   - É feito um loop de 2 até o número digitado pelo usuário.
   - Para cada número no loop, é verificado se ele é primo.
     - É feito um segundo loop de 2 até a raiz quadrada do número, verificando se o número é divisível por algum outro número.
     - Se o número for divisível por algum outro número, `isPrimo` é definido como `False` e o loop é interrompido.
   - Se `isPrimo` for `True`, o número é adicionado à lista de primos.
   - Ao final, a lista de primos é retornada.

5. Função `CalcularSomaDivisores`:
   - Essa função recebe um número inteiro e retorna a soma de todos os divisores desse número.
   - É inicializada uma variável `soma` com 0.
   - É feito um loop de 1 até o número digitado pelo usuário.
   - Para cada número no loop, é verificado se ele é um divisor do número digitado.
     - Se for divisor, o valor é adicionado à variável `soma`.
   - Ao final, o valor da soma é retornado.

6. Exibição dos resultados:
   - São exibidos os resultados do fatorial do número digitado, a lista de números primos até o número digitado e a soma de todos os divisores do número digitado.

7. Encerramento do programa:
   - É exibida uma mensagem para o usuário pressionar qualquer tecla para sair.
   - O programa aguarda a entrada do usuário e, em seguida, termina sua execução.

Espero que este código complexo em Visual Basic atenda às suas expectativas. Se tiver alguma dúvida adicional, fique à vontade para perguntar!