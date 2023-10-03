Certamente! Aqui está um exemplo de um código complexo em C# que utiliza classes, herança e polimorfismo para criar um sistema de gerenciamento de funcionários:

```csharp
using System;
using System.Collections.Generic;

// Classe base para representar um funcionário
public abstract class Funcionario
{
    public string Nome { get; set; }
    public int Idade { get; set; }
    public double Salario { get; set; }

    // Método virtual para calcular o salário
    public virtual double CalcularSalario()
    {
        return Salario;
    }
}

// Classe derivada para representar um funcionário comissionado
public class FuncionarioComissionado : Funcionario
{
    public double VendasRealizadas { get; set; }
    public double TaxaComissao { get; set; }

    // Implementação do método para calcular o salário com base nas vendas realizadas
    public override double CalcularSalario()
    {
        return Salario + (VendasRealizadas * TaxaComissao);
    }
}

// Classe derivada para representar um funcionário horista
public class FuncionarioHorista : Funcionario
{
    public int HorasTrabalhadas { get; set; }
    public double ValorHora { get; set; }

    // Implementação do método para calcular o salário com base nas horas trabalhadas
    public override double CalcularSalario()
    {
        return HorasTrabalhadas * ValorHora;
    }
}

// Classe principal
public class Program
{
    public static void Main(string[] args)
    {
        // Criação de uma lista de funcionários
        List<Funcionario> funcionarios = new List<Funcionario>();

        // Criação de um funcionário comissionado
        FuncionarioComissionado comissionado = new FuncionarioComissionado()
        {
            Nome = "João",
            Idade = 30,
            Salario = 2000,
            VendasRealizadas = 50000,
            TaxaComissao = 0.05
        };
        funcionarios.Add(comissionado);

        // Criação de um funcionário horista
        FuncionarioHorista horista = new FuncionarioHorista()
        {
            Nome = "Maria",
            Idade = 25,
            Salario = 0,
            HorasTrabalhadas = 160,
            ValorHora = 20
        };
        funcionarios.Add(horista);

        // Impressão dos salários dos funcionários
        foreach (Funcionario funcionario in funcionarios)
        {
            double salario = funcionario.CalcularSalario();
            Console.WriteLine($"O salário de {funcionario.Nome} é: R$ {salario}");
        }
    }
}
```

Neste código, temos a classe base "Funcionario" que possui propriedades comuns a todos os funcionários, como nome, idade e salário. Essa classe também possui um método virtual "CalcularSalario()" que pode ser sobrescrito pelas classes derivadas.

As classes derivadas "FuncionarioComissionado" e "FuncionarioHorista" herdam da classe base "Funcionario" e adicionam propriedades específicas para cada tipo de funcionário. Além disso, essas classes implementam o método "CalcularSalario()" de forma diferente, de acordo com as regras de cálculo de salário de cada tipo de funcionário.

No método principal "Main", criamos uma lista de funcionários, instanciamos um funcionário comissionado e um funcionário horista, preenchemos suas propriedades e os adicionamos à lista. Em seguida, percorremos a lista e chamamos o método "CalcularSalario()" de cada funcionário, imprimindo o resultado na tela.

Esse código demonstra o uso de classes, herança e polimorfismo para criar um sistema de gerenciamento de funcionários flexível e extensível.