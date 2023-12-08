Claro! Aqui está um código em C# que implementa um sistema de cadastro de funcionários utilizando uma abordagem orientada a objetos, com banco de dados em memória.

```csharp
using System;
using System.Collections.Generic;

namespace CadastroFuncionarios
{
    class Funcionario
    {
        public int Id { get; set; }
        public string Nome { get; set; }
        public string Cargo { get; set; }
        public double Salario { get; set; }
    }

    class Program
    {
        static List<Funcionario> funcionarios = new List<Funcionario>();

        static void Main(string[] args)
        {
            bool executando = true;
            do
            {
                Console.WriteLine("========== Sistema de Cadastro de Funcionários ==========");
                Console.WriteLine("[1] Cadastrar novo funcionário");
                Console.WriteLine("[2] Listar todos os funcionários");
                Console.WriteLine("[3] Buscar funcionário por ID");
                Console.WriteLine("[4] Atualizar informações de um funcionário");
                Console.WriteLine("[5] Remover funcionário");
                Console.WriteLine("[0] Sair\n");

                Console.Write("Selecione uma opção: ");
                int opcao = int.Parse(Console.ReadLine());
                Console.WriteLine("");

                switch (opcao)
                {
                    case 1:
                        CadastrarFuncionario();
                        break;
                    case 2:
                        ListarFuncionarios();
                        break;
                    case 3:
                        BuscarFuncionario();
                        break;
                    case 4:
                        AtualizarFuncionario();
                        break;
                    case 5:
                        RemoverFuncionario();
                        break;
                    case 0:
                        executando = false;
                        break;
                    default:
                        Console.WriteLine("Opção inválida!\n");
                        break;
                }
            } while (executando);
        }

        static void CadastrarFuncionario()
        {
            Console.WriteLine("========= Cadastro de Funcionário =========\n");
            Console.Write("Digite o ID: ");
            int id = int.Parse(Console.ReadLine());

            Console.Write("Digite o nome: ");
            string nome = Console.ReadLine();

            Console.Write("Digite o cargo: ");
            string cargo = Console.ReadLine();

            Console.Write("Digite o salário: ");
            double salario = double.Parse(Console.ReadLine());

            Funcionario funcionario = new Funcionario()
            {
                Id = id,
                Nome = nome,
                Cargo = cargo,
                Salario = salario
            };

            funcionarios.Add(funcionario);

            Console.WriteLine("\nFuncionário cadastrado com sucesso!\n");
        }

        static void ListarFuncionarios()
        {
            Console.WriteLine("========= Lista de Funcionários =========\n");

            if (funcionarios.Count == 0)
            {
                Console.WriteLine("Não há funcionários cadastrados.\n");
            }
            else
            {
                foreach (Funcionario funcionario in funcionarios)
                {
                    Console.WriteLine($"ID: {funcionario.Id}");
                    Console.WriteLine($"Nome: {funcionario.Nome}");
                    Console.WriteLine($"Cargo: {funcionario.Cargo}");
                    Console.WriteLine($"Salário: {funcionario.Salario}\n");
                }
            }
        }

        static void BuscarFuncionario()
        {
            Console.WriteLine("========= Buscar Funcionário por ID =========\n");
            Console.Write("Digite o ID do funcionário: ");
            int id = int.Parse(Console.ReadLine());

            Funcionario funcionarioEncontrado = funcionarios.Find(x => x.Id == id);

            if (funcionarioEncontrado != null)
            {
                Console.WriteLine($"ID: {funcionarioEncontrado.Id}");
                Console.WriteLine($"Nome: {funcionarioEncontrado.Nome}");
                Console.WriteLine($"Cargo: {funcionarioEncontrado.Cargo}");
                Console.WriteLine($"Salário: {funcionarioEncontrado.Salario}\n");
            }
            else
            {
                Console.WriteLine("Funcionário não encontrado.\n");
            }
        }

        static void AtualizarFuncionario()
        {
            Console.WriteLine("========= Atualizar Informações de um Funcionário =========\n");
            Console.Write("Digite o ID do funcionário: ");
            int id = int.Parse(Console.ReadLine());

            Funcionario funcionarioEncontrado = funcionarios.Find(x => x.Id == id);

            if (funcionarioEncontrado != null)
            {
                Console.Write("Digite o novo nome: ");
                string novoNome = Console.ReadLine();
                funcionarioEncontrado.Nome = novoNome;

                Console.Write("Digite o novo cargo: ");
                string novoCargo = Console.ReadLine();
                funcionarioEncontrado.Cargo = novoCargo;

                Console.Write("Digite o novo salário: ");
                double novoSalario = double.Parse(Console.ReadLine());
                funcionarioEncontrado.Salario = novoSalario;

                Console.WriteLine("Informações do funcionário atualizadas com sucesso!\n");
            }
            else
            {
                Console.WriteLine("Funcionário não encontrado.\n");
            }
        }

        static void RemoverFuncionario()
        {
            Console.WriteLine("========= Remover Funcionário =========\n");
            Console.Write("Digite o ID do funcionário: ");
            int id = int.Parse(Console.ReadLine());

            Funcionario funcionarioEncontrado = funcionarios.Find(x => x.Id == id);

            if (funcionarioEncontrado != null)
            {
                funcionarios.Remove(funcionarioEncontrado);
                Console.WriteLine("Funcionário removido com sucesso!\n");
            }
            else
            {
                Console.WriteLine("Funcionário não encontrado.\n");
            }
        }
    }
}
```

Este código implementa um sistema de cadastro de funcionários que permite cadastrar, listar, buscar, atualizar e remover funcionários. 

A classe `Funcionario` representa um funcionário com propriedades como ID, Nome, Cargo e Salário. A lista `funcionarios` armazena todos os funcionários cadastrados em memória.

O programa principal (`Main`) exibe um menu de opções e interage com o usuário para realizar as operações desejadas. As opções incluem cadastrar um novo funcionário, listar todos os funcionários, buscar um funcionário pelo ID, atualizar as informações de um funcionário e remover um funcionário. A opção 0 permite sair do programa.

Cada opção é implementada em um método separado, como `CadastrarFuncionario()`, `ListarFuncionarios()`, `BuscarFuncionario()`, `AtualizarFuncionario()` e `RemoverFuncionario()`. Esses métodos realizam as operações correspondentes e interagem com o usuário por meio da entrada e saída padrão (`Console`).

Espero que este código ajude a entender como construir um sistema de cadastro de funcionários em C#.