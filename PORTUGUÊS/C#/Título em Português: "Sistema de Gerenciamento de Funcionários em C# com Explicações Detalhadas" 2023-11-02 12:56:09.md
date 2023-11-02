Certamente, vou criar um código complexo em C# que envolve a criação de um sistema de gerenciamento de funcionários em uma empresa fictícia. Este código estará em português e será acompanhado de explicações detalhadas. Lembre-se de que, devido à limitação de espaço, o código será uma versão simplificada e não incluirá todas as funcionalidades necessárias em um sistema de gerenciamento real. Vamos lá:

```csharp
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        List<Funcionario> funcionarios = new List<Funcionario>();

        while (true)
        {
            Console.WriteLine("Sistema de Gerenciamento de Funcionários");
            Console.WriteLine("1. Adicionar Funcionário");
            Console.WriteLine("2. Listar Funcionários");
            Console.WriteLine("3. Sair");
            Console.Write("Escolha uma opção: ");

            int escolha = int.Parse(Console.ReadLine());

            switch (escolha)
            {
                case 1:
                    AdicionarFuncionario(funcionarios);
                    break;

                case 2:
                    ListarFuncionarios(funcionarios);
                    break;

                case 3:
                    Console.WriteLine("O programa foi encerrado.");
                    return;

                default:
                    Console.WriteLine("Opção inválida. Tente novamente.");
                    break;
            }
        }
    }

    static void AdicionarFuncionario(List<Funcionario> funcionarios)
    {
        Console.WriteLine("Adicionar um novo funcionário");
        Console.Write("Nome: ");
        string nome = Console.ReadLine();
        Console.Write("Cargo: ");
        string cargo = Console.ReadLine();
        Console.Write("Salário: ");
        double salario = double.Parse(Console.ReadLine());

        Funcionario novoFuncionario = new Funcionario(nome, cargo, salario);
        funcionarios.Add(novoFuncionario);

        Console.WriteLine("Funcionário adicionado com sucesso!");
    }

    static void ListarFuncionarios(List<Funcionario> funcionarios)
    {
        Console.WriteLine("Lista de Funcionários:");
        foreach (var funcionario in funcionarios)
        {
            Console.WriteLine($"Nome: {funcionario.Nome}, Cargo: {funcionario.Cargo}, Salário: {funcionario.Salario:C}");
        }
    }
}

class Funcionario
{
    public string Nome { get; }
    public string Cargo { get; }
    public double Salario { get; }

    public Funcionario(string nome, string cargo, double salario)
    {
        Nome = nome;
        Cargo = cargo;
        Salario = salario;
    }
}
```

Neste código, criamos um sistema de gerenciamento de funcionários que permite adicionar funcionários, listar funcionários e sair do programa. A classe `Funcionario` é usada para armazenar informações sobre os funcionários, como nome, cargo e salário. O programa principal permite ao usuário interagir com o sistema por meio de um menu.

Espero que este código sirva como base para suas necessidades. Se você tiver alguma pergunta ou precisar de mais explicações, por favor, me avise.