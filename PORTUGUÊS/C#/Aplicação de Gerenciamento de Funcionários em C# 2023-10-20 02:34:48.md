Claro! Aqui está um exemplo de um código complexo em C# que envolve uma aplicação de gerenciamento de funcionários em uma empresa:

```csharp
using System;
using System.Collections.Generic;

namespace GerenciamentoFuncionarios
{
    class Program
    {
        static void Main(string[] args)
        {
            List<Funcionario> listaFuncionarios = new List<Funcionario>();

            bool continuar = true;

            while (continuar)
            {
                Console.WriteLine("----- Gerenciamento de Funcionários -----");
                Console.WriteLine("Selecione uma opção:");
                Console.WriteLine("1. Cadastrar funcionário");
                Console.WriteLine("2. Listar funcionários");
                Console.WriteLine("3. Remover funcionário");
                Console.WriteLine("4. Sair");

                int opcao = int.Parse(Console.ReadLine());

                switch (opcao)
                {
                    case 1:
                        Console.WriteLine("Digite o nome do funcionário:");
                        string nome = Console.ReadLine();
                        Console.WriteLine("Digite o cargo do funcionário:");
                        string cargo = Console.ReadLine();
                        Console.WriteLine("Digite o salário do funcionário:");
                        double salario = double.Parse(Console.ReadLine());

                        Funcionario novoFuncionario = new Funcionario(nome, cargo, salario);
                        listaFuncionarios.Add(novoFuncionario);

                        Console.WriteLine("Funcionário cadastrado com sucesso!");
                        break;

                    case 2:
                        Console.WriteLine("----- Lista de Funcionários -----");
                        foreach (Funcionario funcionario in listaFuncionarios)
                        {
                            Console.WriteLine(funcionario.ToString());
                        }
                        break;

                    case 3:
                        Console.WriteLine("Digite o nome do funcionário que deseja remover:");
                        string nomeRemover = Console.ReadLine();

                        Funcionario funcionarioRemover = listaFuncionarios.Find(funcionario => funcionario.Nome == nomeRemover);

                        if (funcionarioRemover != null)
                        {
                            listaFuncionarios.Remove(funcionarioRemover);
                            Console.WriteLine("Funcionário removido com sucesso!");
                        }
                        else
                        {
                            Console.WriteLine("Funcionário não encontrado!");
                        }
                        break;

                    case 4:
                        continuar = false;
                        Console.WriteLine("Encerrando o programa...");
                        break;

                    default:
                        Console.WriteLine("Opção inválida!");
                        break;
                }

                Console.WriteLine();
            }
        }
    }

    class Funcionario
    {
        public string Nome { get; set; }
        public string Cargo { get; set; }
        public double Salario { get; set; }

        public Funcionario(string nome, string cargo, double salario)
        {
            Nome = nome;
            Cargo = cargo;
            Salario = salario;
        }

        public override string ToString()
        {
            return $"Nome: {Nome} | Cargo: {Cargo} | Salário: R$ {Salario:F2}";
        }
    }
}
```

Neste código, temos uma classe `Funcionario` que representa um funcionário da empresa, contendo os atributos `Nome`, `Cargo` e `Salario`. Essa classe também possui um método `ToString()` sobrescrito para formatar a exibição dos dados do funcionário.

A classe `Program` é a classe principal que contém o método `Main`. Neste método, temos uma lista de funcionários `listaFuncionarios` e um loop `while` para exibir um menu de opções para o usuário.

No menu, as opções são:

1. Cadastrar funcionário: O usuário pode digitar o nome, cargo e salário de um funcionário para ser cadastrado na lista de funcionários.
2. Listar funcionários: Exibe a lista de funcionários cadastrados, utilizando o método `ToString()` sobrescrito na classe `Funcionario` para formatar a exibição dos dados.
3. Remover funcionário: Solicita ao usuário o nome do funcionário que deseja remover e procura na lista de funcionários pelo nome informado. Caso encontrado, o funcionário é removido da lista.
4. Sair: Encerra o programa.

O programa utiliza estruturas de controle como `switch` e `foreach` para realizar as diversas funcionalidades do gerenciamento de funcionários.

Esse é apenas um exemplo de código complexo em C#. Existem inúmeras possibilidades e formas de estruturar um programa, dependendo dos requisitos específicos do projeto.