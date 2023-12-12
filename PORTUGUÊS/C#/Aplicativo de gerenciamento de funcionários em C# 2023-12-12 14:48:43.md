Claro! Aqui está um exemplo de código complexo em C#, que envolve a criação de uma aplicação de gerenciamento de funcionários em uma empresa:

```csharp
using System;
using System.Collections.Generic;

namespace GerenciamentoFuncionarios
{
    class Program
    {
        static void Main(string[] args)
        {
            List<Funcionario> funcionarios = new List<Funcionario>();

            bool executando = true;
            while (executando)
            {
                Console.WriteLine("------------------------------");
                Console.WriteLine("GERENCIAMENTO DE FUNCIONÁRIOS");
                Console.WriteLine("------------------------------");
                Console.WriteLine("1 - Adicionar funcionário");
                Console.WriteLine("2 - Listar funcionários");
                Console.WriteLine("3 - Remover funcionário");
                Console.WriteLine("4 - Sair");
                Console.WriteLine();

                Console.Write("Digite a opção desejada: ");
                int opcao = Convert.ToInt32(Console.ReadLine());

                switch (opcao)
                {
                    case 1:
                        AdicionarFuncionario(funcionarios);
                        break;
                    case 2:
                        ListarFuncionarios(funcionarios);
                        break;
                    case 3:
                        RemoverFuncionario(funcionarios);
                        break;
                    case 4:
                        executando = false;
                        break;
                    default:
                        Console.WriteLine("Opção inválida. Por favor, tente novamente.");
                        break;
                }

                Console.WriteLine();
            }
        }

        static void AdicionarFuncionario(List<Funcionario> funcionarios)
        {
            Console.WriteLine();
            Console.WriteLine("ADICIONAR FUNCIONÁRIO");
            Console.WriteLine();

            Console.Write("Digite o nome do funcionário: ");
            string nome = Console.ReadLine();

            Console.Write("Digite o cargo do funcionário: ");
            string cargo = Console.ReadLine();

            Console.Write("Digite o salário do funcionário: ");
            double salario = Convert.ToDouble(Console.ReadLine());

            Funcionario novoFuncionario = new Funcionario(nome, cargo, salario);
            funcionarios.Add(novoFuncionario);

            Console.WriteLine();
            Console.WriteLine("Funcionário adicionado com sucesso!");
        }

        static void ListarFuncionarios(List<Funcionario> funcionarios)
        {
            Console.WriteLine();
            Console.WriteLine("LISTA DE FUNCIONÁRIOS");
            Console.WriteLine();

            if (funcionarios.Count == 0)
            {
                Console.WriteLine("Não há funcionários cadastrados.");
            }
            else
            {
                foreach (var funcionario in funcionarios)
                {
                    Console.WriteLine("-----------------------------------");
                    Console.WriteLine($"Nome: {funcionario.Nome}");
                    Console.WriteLine($"Cargo: {funcionario.Cargo}");
                    Console.WriteLine($"Salário: R${funcionario.Salario}");
                    Console.WriteLine("-----------------------------------");
                }
            }
        }

        static void RemoverFuncionario(List<Funcionario> funcionarios)
        {
            Console.WriteLine();
            Console.WriteLine("REMOVER FUNCIONÁRIO");
            Console.WriteLine();

            if (funcionarios.Count == 0)
            {
                Console.WriteLine("Não há funcionários cadastrados.");
            }
            else
            {
                Console.Write("Digite o nome do funcionário que deseja remover: ");
                string nome = Console.ReadLine();

                bool encontrado = false;
                foreach (var funcionario in funcionarios)
                {
                    if (funcionario.Nome.ToLower() == nome.ToLower())
                    {
                        funcionarios.Remove(funcionario);
                        encontrado = true;
                        Console.WriteLine("Funcionário removido com sucesso!");
                        break;
                    }
                }

                if (!encontrado)
                {
                    Console.WriteLine("Funcionário não encontrado.");
                }
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
    }
}
```

Neste código, criei uma aplicação de gerenciamento de funcionários em uma empresa. A aplicação permite ao usuário adicionar funcionários, listar os funcionários cadastrados, remover funcionários e sair do programa.

A classe `Program` contém o método `Main`, que é o ponto de entrada da aplicação. Neste método, utilizo um loop `while` para manter o programa em execução até que o usuário escolha a opção de sair.

Dentro do loop, exibo um menu com diferentes opções para o usuário selecionar. As opções são tratadas em um bloco `switch`, onde cada caso corresponde a uma funcionalidade específica, como adicionar, listar ou remover funcionários.

As funcionalidades são implementadas em métodos separados, como `AdicionarFuncionario`, `ListarFuncionarios` e `RemoverFuncionario`. Esses métodos recebem a lista de funcionários como parâmetro e realizam as operações correspondentes.

A classe `Funcionario` define a estrutura de um funcionário, com as propriedades `Nome`, `Cargo` e `Salario`. O construtor da classe é responsável por inicializar essas propriedades com os valores fornecidos.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou aqui para ajudar.