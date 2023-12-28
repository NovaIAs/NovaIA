Claro, vou criar um código complexo em Visual Basic que envolve a criação de um sistema de gerenciamento de funcionários para uma empresa. Esse sistema permitirá adicionar, editar e excluir funcionários, além de exibir relatórios de funcionários ativos e inativos. Vou explicar o código em detalhes:

```vb
Imports System

Module GerenciamentoFuncionario
    'Criando uma classe para representar o funcionário
    Public Class Funcionario
        Public Property ID As Integer
        Public Property Nome As String
        Public Property Cargo As String
        Public Property Salario As Decimal
        Public Property Ativo As Boolean
    End Class

    'Criando uma lista para armazenar os funcionários
    Public Funcionarios As New List(Of Funcionario)()

    Sub Main()
        Dim opcao As Integer

        Do
            Console.Clear()
            Console.WriteLine("1 - Adicionar funcionário")
            Console.WriteLine("2 - Editar funcionário")
            Console.WriteLine("3 - Excluir funcionário")
            Console.WriteLine("4 - Exibir relatório de funcionários ativos")
            Console.WriteLine("5 - Exibir relatório de funcionários inativos")
            Console.WriteLine("6 - Sair")
            Console.Write("Escolha uma opção: ")
            opcao = Integer.Parse(Console.ReadLine())

            Select Case opcao
                Case 1
                    AdicionarFuncionario()
                Case 2
                    EditarFuncionario()
                Case 3
                    ExcluirFuncionario()
                Case 4
                    ExibirRelatorioAtivos()
                Case 5
                    ExibirRelatorioInativos()
            End Select
        Loop While opcao <> 6
    End Sub

    Sub AdicionarFuncionario()
        Dim funcionario As New Funcionario()

        Console.WriteLine("Adicionar Funcionário")
        Console.Write("ID: ")
        funcionario.ID = Integer.Parse(Console.ReadLine())
        Console.Write("Nome: ")
        funcionario.Nome = Console.ReadLine()
        Console.Write("Cargo: ")
        funcionario.Cargo = Console.ReadLine()
        Console.Write("Salário: R$")
        funcionario.Salario = Decimal.Parse(Console.ReadLine())
        funcionario.Ativo = True

        Funcionarios.Add(funcionario)
        Console.WriteLine("Funcionário adicionado com sucesso!")
        Console.ReadKey()
    End Sub

    Sub EditarFuncionario()
        Console.WriteLine("Editar Funcionário")
        Console.Write("Digite o ID do funcionário: ")
        Dim id As Integer = Integer.Parse(Console.ReadLine())

        Dim funcionario As Funcionario = Funcionarios.Find(Function(f) f.ID = id)

        If funcionario IsNot Nothing Then
            Console.Write("Nome: ")
            funcionario.Nome = Console.ReadLine()
            Console.Write("Cargo: ")
            funcionario.Cargo = Console.ReadLine()
            Console.Write("Salário: R$")
            funcionario.Salario = Decimal.Parse(Console.ReadLine())

            Console.WriteLine("Funcionário editado com sucesso!")
        Else
            Console.WriteLine("Funcionário não encontrado")
        End If

        Console.ReadKey()
    End Sub

    Sub ExcluirFuncionario()
        Console.WriteLine("Excluir Funcionário")
        Console.Write("Digite o ID do funcionário: ")
        Dim id As Integer = Integer.Parse(Console.ReadLine())

        Dim funcionario As Funcionario = Funcionarios.Find(Function(f) f.ID = id)

        If funcionario IsNot Nothing Then
            Funcionarios.Remove(funcionario)
            Console.WriteLine("Funcionário excluído com sucesso!")
        Else
            Console.WriteLine("Funcionário não encontrado")
        End If

        Console.ReadKey()
    End Sub

    Sub ExibirRelatorioAtivos()
        Console.WriteLine("Relatório de Funcionários Ativos")
        Console.WriteLine("---------------------------------")

        For Each funcionario As Funcionario In Funcionarios
            If funcionario.Ativo Then
                Console.WriteLine($"ID: {funcionario.ID}")
                Console.WriteLine($"Nome: {funcionario.Nome}")
                Console.WriteLine($"Cargo: {funcionario.Cargo}")
                Console.WriteLine($"Salário: R${funcionario.Salario}")
                Console.WriteLine("---------------------------------")
            End If
        Next

        Console.ReadKey()
    End Sub

    Sub ExibirRelatorioInativos()
        Console.WriteLine("Relatório de Funcionários Inativos")
        Console.WriteLine("-----------------------------------")

        For Each funcionario As Funcionario In Funcionarios
            If Not funcionario.Ativo Then
                Console.WriteLine($"ID: {funcionario.ID}")
                Console.WriteLine($"Nome: {funcionario.Nome}")
                Console.WriteLine($"Cargo: {funcionario.Cargo}")
                Console.WriteLine($"Salário: R${funcionario.Salario}")
                Console.WriteLine("-----------------------------------")
            End If
        Next

        Console.ReadKey()
    End Sub
End Module
```

Neste código, temos um módulo chamado "GerenciamentoFuncionario" que contém uma classe chamada "Funcionario" para representar os funcionários da empresa. Além disso, temos uma lista chamada "Funcionarios" para armazenar os funcionários cadastrados.

O programa principal é o "Main", que exibe um menu de opções para o usuário. Ele utiliza uma estrutura de repetição "Do...Loop" para manter o menu sendo exibido até que o usuário escolha a opção "Sair".

Cada opção do menu é implementada em um submódulo separado. Por exemplo, o submódulo "AdicionarFuncionario" permite ao usuário adicionar um novo funcionário, solicitando as informações necessárias e adicionando-o à lista de funcionários.

Os outros submódulos são responsáveis por implementar as funcionalidades de editar, excluir e exibir relatórios de funcionários ativos e inativos. Eles utilizam recursos como pesquisa na lista de funcionários e laços de repetição para percorrer e exibir os dados.

Espero que esse exemplo atenda às suas expectativas. Se você tiver alguma dúvida ou precisar de mais detalhes, estou à disposição para ajudar.