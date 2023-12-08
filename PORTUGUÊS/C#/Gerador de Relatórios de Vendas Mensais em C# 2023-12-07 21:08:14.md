Claro! Aqui está um código complexo em C#:

```csharp
using System;
using System.Collections.Generic;
using System.IO;

namespace ProgramaNovaIA
{
    public class GeradorDeRelatorios
    {
        public static void Main()
        {
            Console.WriteLine("Bem-vindo ao Gerador de Relatórios!");

            List<Cliente> clientes = new List<Cliente>();
            LerDadosClientes(clientes);

            Console.WriteLine("Relatório de Vendas Mensal");
            Console.WriteLine("--------------------------");

            foreach (Cliente cliente in clientes)
            {
                Console.WriteLine($"Cliente: {cliente.Nome}");
                Console.WriteLine("Vendas:");
                foreach (Venda venda in cliente.Vendas)
                {
                    Console.WriteLine($"- Produto: {venda.Produto}, Valor: {venda.Valor}");
                }

                Console.WriteLine("--------------------------");
            }

            Console.WriteLine("Relatório Gerado com Sucesso. Pressione ENTER para sair.");
            Console.ReadLine();
        }

        private static void LerDadosClientes(List<Cliente> clientes)
        {
            string arquivo = "dados_clientes.txt";
            if (File.Exists(arquivo))
            {
                using (StreamReader sr = new StreamReader(arquivo))
                {
                    string linha;
                    while ((linha = sr.ReadLine()) != null)
                    {
                        string[] dados = linha.Split(';');
                        string nome = dados[0];
                        string[] vendaProdutos = dados[1].Split(',');
                        string[] vendaValores = dados[2].Split(',');

                        Cliente cliente = new Cliente(nome);
                        for (int i = 0; i < vendaProdutos.Length; i++)
                        {
                            string produto = vendaProdutos[i];
                            double valor = Convert.ToDouble(vendaValores[i]);

                            Venda venda = new Venda(produto, valor);
                            cliente.Vendas.Add(venda);
                        }

                        clientes.Add(cliente);
                    }
                }
            }
            else
            {
                Console.WriteLine("Erro: arquivo de dados dos clientes não encontrado.");
            }
        }
    }

    public class Cliente
    {
        public string Nome { get; private set; }
        public List<Venda> Vendas { get; private set; }

        public Cliente(string nome)
        {
            Nome = nome;
            Vendas = new List<Venda>();
        }
    }

    public class Venda
    {
        public string Produto { get; private set; }
        public double Valor { get; private set; }

        public Venda(string produto, double valor)
        {
            Produto = produto;
            Valor = valor;
        }
    }
}
```

Este código simula um gerador de relatórios de vendas mensais para um sistema de gerenciamento de clientes. Aqui está o que acontece neste código:

1. O programa começa imprimindo uma mensagem de boas-vindas para o usuário.
2. Uma lista de clientes vazia é criada.
3. O método `LerDadosClientes` é chamado para ler os dados dos clientes a partir de um arquivo texto chamado `dados_clientes.txt`. Os dados do cliente são separados por ";" e as vendas do cliente são separadas por ",".
4. O programa inicia a impressão do relatório de vendas mensal, percorrendo cada cliente da lista.
5. Para cada cliente, o programa imprime o nome do cliente e, em seguida, percorre todas as vendas do cliente, imprimindo o produto vendido e o valor.
6. Após imprimir todas as vendas do cliente, o programa imprime uma linha de separação.
7. Ao final, é exibida uma mensagem de sucesso na geração do relatório e o programa espera que o usuário pressione ENTER para sair.

O código também possui três classes: `GeradorDeRelatorios`, `Cliente` e `Venda`. 

A classe `Cliente` representa um cliente e possui um nome e uma lista de vendas.
A classe `Venda` representa uma venda e possui um produto e um valor.

Essas classes são usadas para estruturar os dados lidos a partir do arquivo, facilitando a exibição do relatório de vendas. O código também trata o cenário onde o arquivo não é encontrado, mostrando uma mensagem de erro.

Espero que este código atenda às suas expectativas!