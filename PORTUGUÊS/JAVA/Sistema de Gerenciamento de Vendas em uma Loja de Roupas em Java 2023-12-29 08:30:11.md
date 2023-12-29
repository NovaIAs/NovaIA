Claro! Aqui está um código complexo em Java, que consiste em um sistema de gerenciamento de vendas para uma loja de roupas. O código é estruturado em classes e implementa diversas funcionalidades, como cadastro de produtos, registro de vendas, geração de relatórios, entre outros. Vou explicar cada parte do código para você:

```java
import java.util.ArrayList;
import java.util.Scanner;

public class LojaDeRoupas {
    private ArrayList<Produto> produtos;
    private ArrayList<Venda> vendas;
    
    public LojaDeRoupas() {
        produtos = new ArrayList<>();
        vendas = new ArrayList<>();
    }
    
    public void cadastrarProduto() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Cadastro de Produto");
        System.out.println("-------------------");
        
        System.out.print("Nome: ");
        String nome = scanner.nextLine();
        
        System.out.print("Preço: ");
        double preco = scanner.nextDouble();
        
        Produto produto = new Produto(nome, preco);
        produtos.add(produto);
        
        System.out.println("Produto cadastrado com sucesso!");
    }
    
    public void realizarVenda() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Realização de Venda");
        System.out.println("-------------------");
        
        System.out.print("Informe o CPF do cliente: ");
        String cpf = scanner.nextLine();
        
        System.out.print("Informe o nome do produto: ");
        String nomeProduto = scanner.nextLine();
        
        Produto produto = null;
        
        for (Produto p : produtos) {
            if (p.getNome().equalsIgnoreCase(nomeProduto)) {
                produto = p;
                break;
            }
        }
        
        if (produto == null) {
            System.out.println("Produto não encontrado!");
            return;
        }
        
        System.out.print("Informe a quantidade: ");
        int quantidade = scanner.nextInt();
        
        Venda venda = new Venda(cpf, produto, quantidade);
        vendas.add(venda);
        
        System.out.println("Venda registrada com sucesso!");
    }
    
    public void gerarRelatorio() {
        System.out.println("Relatório de Vendas");
        System.out.println("-------------------");
        
        for (Venda venda : vendas) {
            System.out.println(venda.toString());
        }
        
        System.out.println("-------------------");
        System.out.println("Total de vendas: " + vendas.size());
    }
    
    public static void main(String[] args) {
        LojaDeRoupas loja = new LojaDeRoupas();
        
        Scanner scanner = new Scanner(System.in);
        int opcao = 0;
        
        while (opcao != 4) {
            System.out.println("1 - Cadastrar Produto");
            System.out.println("2 - Realizar Venda");
            System.out.println("3 - Gerar Relatório");
            System.out.println("4 - Sair");
            System.out.print("Opção: ");
            opcao = scanner.nextInt();
            scanner.nextLine();
            
            switch (opcao) {
                case 1:
                    loja.cadastrarProduto();
                    break;
                case 2:
                    loja.realizarVenda();
                    break;
                case 3:
                    loja.gerarRelatorio();
                    break;
                case 4:
                    System.out.println("Encerrando o programa...");
                    break;
                default:
                    System.out.println("Opção inválida!");
                    break;
            }
            
            System.out.println();
        }
    }
}

class Produto {
    private String nome;
    private double preco;
    
    public Produto(String nome, double preco) {
        this.nome = nome;
        this.preco = preco;
    }
    
    public String getNome() {
        return nome;
    }
    
    public double getPreco() {
        return preco;
    }
    
    @Override
    public String toString() {
        return "Nome: " + nome + ", Preço: " + preco;
    }
}

class Venda {
    private String cpfCliente;
    private Produto produto;
    private int quantidade;
    
    public Venda(String cpfCliente, Produto produto, int quantidade) {
        this.cpfCliente = cpfCliente;
        this.produto = produto;
        this.quantidade = quantidade;
    }
    
    public String getCpfCliente() {
        return cpfCliente;
    }
    
    public Produto getProduto() {
        return produto;
    }
    
    public int getQuantidade() {
        return quantidade;
    }
    
    public double getValorTotal() {
        return produto.getPreco() * quantidade;
    }
    
    @Override
    public String toString() {
        return "CPF Cliente: " + cpfCliente + ", Produto: " + produto.getNome() + ", Quantidade: " + quantidade + ", Valor Total: " + getValorTotal();
    }
}
```

Nesse código, temos a classe `LojaDeRoupas`, que é a classe principal do sistema. Ela possui os métodos `cadastrarProduto()`, `realizarVenda()`, `gerarRelatorio()` e o método `main()`. O método `cadastrarProduto()` permite cadastrar um novo produto, solicitando ao usuário o nome e o preço do produto. O método `realizarVenda()` realiza uma venda, solicitando ao usuário o CPF do cliente e o nome do produto, buscando o produto cadastrado e registrando a venda. O método `gerarRelatorio()` exibe um relatório de todas as vendas realizadas, mostrando o CPF do cliente, o nome do produto, a quantidade e o valor total da venda. 

As classes `Produto` e `Venda` são classes auxiliares utilizadas para representar os produtos e as vendas. A classe `Produto` possui os atributos `nome` e `preco`, além de um construtor, métodos getters e um método `toString()` para exibir as informações do produto. A classe `Venda` possui os atributos `cpfCliente`, `produto`, `quantidade`, além de um construtor, métodos getters e um método `toString()` para exibir as informações da venda, incluindo o valor total.

No método `main()`, é criada uma instância da classe `LojaDeRoupas` e é exibido um menu para o usuário, onde ele pode escolher entre cadastrar um produto, realizar uma venda, gerar um relatório ou sair do programa. O código utiliza um loop while para manter o programa em execução até que o usuário escolha a opção de sair.

Espero que esse código atenda às suas expectativas!