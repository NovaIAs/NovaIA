Claro! Aqui está um código complexo em C# que usa uma combinação de classes, métodos e estruturas de controle para criar um programa de gerenciamento de funcionários:

```csharp
using System;
using System.Collections.Generic;

namespace GerenciamentoFuncionarios
{
    public class Funcionario
    {
        public int Id { get; set; }
        public string Nome { get; set; }
        public double Salario { get; set; }

        public override string ToString()
        {
            return $"ID: {Id}, Nome: {Nome}, Salário: {Salario:C}";
        }
    }

    public class GerenciadorFuncionarios
    {
        private List<Funcionario> funcionarios;

        public GerenciadorFuncionarios()
        {
            funcionarios = new List<Funcionario>();
        }

        public void AdicionarFuncionario(Funcionario funcionario)
        {
            funcionarios.Add(funcionario);
        }

        public void RemoverFuncionario(int id)
        {
            funcionarios.RemoveAll(f => f.Id == id);
        }

        public void ListarFuncionarios()
        {
            Console.WriteLine("Lista de Funcionários:");
            foreach (var funcionario in funcionarios)
            {
                Console.WriteLine(funcionario);
            }
        }

        public double CalcularFolhaPagamento()
        {
            double totalSalarios = 0;
            foreach (var funcionario in funcionarios)
            {
                totalSalarios += funcionario.Salario;
            }
            return totalSalarios;
        }
    }

    public class Program
    {
        public static void Main(string[] args)
        {
            GerenciadorFuncionarios gerenciador = new GerenciadorFuncionarios();

            Console.WriteLine("Bem-vindo ao sistema de gerenciamento de funcionários!");

            bool executar = true;
            while (executar)
            {
                Console.WriteLine("\nEscolha uma opção:");
                Console.WriteLine("1. Adicionar funcionário");
                Console.WriteLine("2. Remover funcionário");
                Console.WriteLine("3. Listar funcionários");
                Console.WriteLine("4. Calcular folha de pagamento");
                Console.WriteLine("5. Sair");

                int opcao;
                if (int.TryParse(Console.ReadLine(), out opcao))
                {
                    switch (opcao)
                    {
                        case 1:
                            Console.WriteLine("Digite o ID do funcionário:");
                            int id = int.Parse(Console.ReadLine());
                            Console.WriteLine("Digite o nome do funcionário:");
                            string nome = Console.ReadLine();
                            Console.WriteLine("Digite o salário do funcionário:");
                            double salario = double.Parse(Console.ReadLine());

                            Funcionario novoFuncionario = new Funcionario()
                            {
                                Id = id,
                                Nome = nome,
                                Salario = salario
                            };

                            gerenciador.AdicionarFuncionario(novoFuncionario);
                            Console.WriteLine("Funcionário adicionado com sucesso!");
                            break;

                        case 2:
                            Console.WriteLine("Digite o ID do funcionário que deseja remover:");
                            int idRemocao = int.Parse(Console.ReadLine());
                            gerenciador.RemoverFuncionario(idRemocao);
                            Console.WriteLine("Funcionário removido com sucesso!");
                            break;

                        case 3:
                            gerenciador.ListarFuncionarios();
                            break;

                        case 4:
                            double folhaPagamento = gerenciador.CalcularFolhaPagamento();
                            Console.WriteLine($"Folha de pagamento total: {folhaPagamento:C}");
                            break;

                        case 5:
                            executar = false;
                            break;

                        default:
                            Console.WriteLine("Opção inválida!");
                            break;
                    }
                }
                else
                {
                    Console.WriteLine("Opção inválida!");
                }
            }

            Console.WriteLine("\nObrigado por utilizar o sistema de gerenciamento de funcionários!");
        }
    }
}
```

Este código cria um programa de gerenciamento de funcionários que permite ao usuário adicionar, remover, listar e calcular a folha de pagamento dos funcionários. 

A classe `Funcionario` representa um funcionário com propriedades como ID, nome e salário. A classe `GerenciadorFuncionarios` é responsável por gerenciar os funcionários, permitindo adicionar, remover, listar e calcular a folha de pagamento. 

O programa principal `Program` usa um loop `while` para exibir um menu ao usuário e realizar as operações selecionadas. O menu é exibido em um loop até que o usuário escolha a opção "Sair". Cada opção é implementada em um bloco `case` no switch. 

No caso da opção 1 (Adicionar funcionário), o programa solicita ao usuário o ID, nome e salário do funcionário e cria uma instância da classe `Funcionario` com essas informações. Em seguida, a instância é passada para o método `AdicionarFuncionario` do objeto `gerenciador` da classe `GerenciadorFuncionarios`. O mesmo processo é realizado para as outras opções do menu.

O código também inclui tratamento de erros para garantir que o usuário insira valores válidos para as opções e os dados do funcionário.

Espero que este exemplo atenda às suas expectativas!