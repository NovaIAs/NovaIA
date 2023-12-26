Claro! Aqui está um exemplo de um código complexo em C# que realiza uma simulação de um sistema bancário:

```csharp
using System;
using System.Collections.Generic;

namespace SistemaBancario
{
    // Classe para representar uma conta bancária
    public class ContaBancaria
    {
        public int NumeroConta { get; private set; }
        public string Titular { get; private set; }
        public decimal Saldo { get; private set; }

        public ContaBancaria(int numeroConta, string titular, decimal saldoInicial)
        {
            NumeroConta = numeroConta;
            Titular = titular;
            Saldo = saldoInicial;
        }

        public void Depositar(decimal valor)
        {
            Saldo += valor;
            Console.WriteLine($"Depósito de {valor:C} realizado na conta {NumeroConta}. Novo saldo: {Saldo:C}.");
        }

        public void Sacar(decimal valor)
        {
            if (valor <= Saldo)
            {
                Saldo -= valor;
                Console.WriteLine($"Saque de {valor:C} realizado na conta {NumeroConta}. Novo saldo: {Saldo:C}.");
            }
            else
            {
                Console.WriteLine($"Saldo insuficiente na conta {NumeroConta} para realizar o saque de {valor:C}.");
            }
        }

        public override string ToString()
        {
            return $"Conta: {NumeroConta} | Titular: {Titular} | Saldo: {Saldo:C}";
        }
    }

    // Classe principal
    class Program
    {
        static void Main(string[] args)
        {
            // Criação de algumas contas bancárias
            ContaBancaria conta1 = new ContaBancaria(1, "João da Silva", 1000);
            ContaBancaria conta2 = new ContaBancaria(2, "Maria Santos", 500);
            ContaBancaria conta3 = new ContaBancaria(3, "Pedro Costa", 2000);

            // Lista para armazenar as contas
            List<ContaBancaria> contas = new List<ContaBancaria>();
            contas.Add(conta1);
            contas.Add(conta2);
            contas.Add(conta3);

            // Simulação de operações bancárias
            Console.WriteLine("Sistema Bancário - Simulação de Operações");
            Console.WriteLine();

            foreach (ContaBancaria conta in contas)
            {
                Console.WriteLine(conta);
                Console.WriteLine();
                conta.Depositar(100);
                conta.Sacar(500);
                Console.WriteLine();
            }

            Console.ReadKey();
        }
    }
}
```

Neste código, criamos uma classe `ContaBancaria` que representa uma conta bancária com propriedades para o número da conta, o titular e o saldo. A classe também possui métodos para depositar e sacar dinheiro da conta, além de uma sobrescrita do método `ToString()` para exibir as informações da conta de forma legível.

Na classe principal `Program`, criamos algumas instâncias da classe `ContaBancaria` e as armazenamos em uma lista. Em seguida, realizamos uma simulação de operações bancárias, fazendo depósitos e saques em cada conta e exibindo os resultados no console.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, é só me perguntar.