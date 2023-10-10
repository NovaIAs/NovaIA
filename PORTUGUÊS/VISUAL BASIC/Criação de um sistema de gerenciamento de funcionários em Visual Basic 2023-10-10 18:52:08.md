Claro! Aqui está um código complexo em Visual Basic que cria um sistema de gerenciamento de funcionários em uma empresa. O código está comentado em português para facilitar a compreensão:

```vb
Imports System

Module Program
    ' Classe para representar um funcionário
    Class Funcionario
        Public Property Nome As String
        Public Property Cargo As String
        Public Property Salario As Double

        ' Construtor da classe
        Public Sub New(ByVal nome As String, ByVal cargo As String, ByVal salario As Double)
            Me.Nome = nome
            Me.Cargo = cargo
            Me.Salario = salario
        End Sub

        ' Método para exibir as informações do funcionário
        Public Sub ExibirInformacoes()
            Console.WriteLine("Nome: " & Nome)
            Console.WriteLine("Cargo: " & Cargo)
            Console.WriteLine("Salário: " & Salario)
        End Sub
    End Class

    Sub Main(args As String())
        Dim listaFuncionarios As New List(Of Funcionario)() ' Lista para armazenar os funcionários

        ' Adicionando alguns funcionários na lista
        listaFuncionarios.Add(New Funcionario("João", "Desenvolvedor", 5000))
        listaFuncionarios.Add(New Funcionario("Maria", "Gerente", 8000))
        listaFuncionarios.Add(New Funcionario("Pedro", "Analista de Dados", 6000))

        ' Exibindo as informações de todos os funcionários da lista
        For Each funcionario In listaFuncionarios
            funcionario.ExibirInformacoes()
            Console.WriteLine("-------------------------------")
        Next

        Console.ReadLine()
    End Sub
End Module
```

Neste código, criamos uma classe chamada `Funcionario` que representa um funcionário de uma empresa. Essa classe possui três propriedades: `Nome`, `Cargo` e `Salario`. Além disso, ela possui um construtor para inicializar as propriedades e um método chamado `ExibirInformacoes` que imprime as informações do funcionário no console.

No método `Main`, criamos uma lista chamada `listaFuncionarios` para armazenar os funcionários. Em seguida, adicionamos alguns funcionários na lista utilizando o construtor da classe `Funcionario`.

Por fim, percorremos a lista de funcionários utilizando um loop `For Each` e chamamos o método `ExibirInformacoes` para cada funcionário, exibindo suas informações no console.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição para ajudar.