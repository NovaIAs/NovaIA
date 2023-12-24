Aqui está um exemplo de código complexo em Java que envolve a criação de um sistema de gerenciamento de vendas:

```
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class GerenciamentoVendas {
    
    private static List<Produto> estoque = new ArrayList<>();
    private static List<Venda> vendas = new ArrayList<>();
    
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        boolean sair = false;
        while (!sair) {
            System.out.println("----- Sistema de Gerenciamento de Vendas -----");
            System.out.println("1. Cadastrar produto");
            System.out.println("2. Realizar venda");
            System.out.println("3. Exibir estoque");
            System.out.println("4. Exibir vendas");
            System.out.println("5. Sair");
            System.out.print("Escolha uma opção: ");
            
            int opcao = scanner.nextInt();
            scanner.nextLine();
            
            switch (opcao) {
                case 1:
                    cadastrarProduto(scanner);
                    break;
                case 2:
                    realizarVenda(scanner);
                    break;
                case 3:
                    exibirEstoque();
                    break;
                case 4:
                    exibirVendas();
                    break;
                case 5:
                    sair = true;
                    break;
                default:
                    System.out.println("Opção inválida!");
                    break;
            }
            
            System.out.println();
        }
        
        scanner.close();
    }
    
    private static void cadastrarProduto(Scanner scanner) {
        System.out.print("Digite o nome do produto: ");
        String nome = scanner.nextLine();
        
        System.out.print("Digite o preço do produto: ");
        double preco = scanner.nextDouble();
        
        System.out.print("Digite a quantidade em estoque: ");
        int quantidade = scanner.nextInt();
        
        Produto produto = new Produto(nome, preco, quantidade);
        estoque.add(produto);
        
        System.out.println("Produto cadastrado com sucesso!");
    }
    
    private static void realizarVenda(Scanner scanner) {
        System.out.print("Digite o nome do produto a ser vendido: ");
        String nome = scanner.nextLine();
        
        Produto produto = buscarProdutoPorNome(nome);
        if (produto != null) {
            System.out.print("Digite a quantidade a ser vendida: ");
            int quantidade = scanner.nextInt();
            
            if (produto.getQuantidade() >= quantidade) {
                double valorTotal = produto.getPreco() * quantidade;
                
                Venda venda = new Venda(produto, quantidade, valorTotal);
                vendas.add(venda);
                
                produto.setQuantidade(produto.getQuantidade() - quantidade);
                
                System.out.println("Venda realizada com sucesso!");
            } else {
                System.out.println("Quantidade insuficiente em estoque!");
            }
        } else {
            System.out.println("Produto não encontrado!");
        }
    }
    
    private static Produto buscarProdutoPorNome(String nome) {
        for (Produto produto : estoque) {
            if (produto.getNome().equals(nome)) {
                return produto;
            }
        }
        
        return null;
    }
    
    private static void exibirEstoque() {
        System.out.println("----- Estoque -----");
        for (Produto produto : estoque) {
            System.out.println("Nome: " + produto.getNome());
            System.out.println("Preço: R$" + produto.getPreco());
            System.out.println("Quantidade: " + produto.getQuantidade());
            System.out.println("-------------------");
        }
    }
    
    private static void exibirVendas() {
        System.out.println("----- Vendas -----");
        for (Venda venda : vendas) {
            System.out.println("Produto: " + venda.getProduto().getNome());
            System.out.println("Quantidade: " + venda.getQuantidade());
            System.out.println("Valor Total: R$" + venda.getValorTotal());
            System.out.println("-----------------");
        }
    }
    
    private static class Produto {
        private String nome;
        private double preco;
        private int quantidade;
        
        public Produto(String nome, double preco, int quantidade) {
            this.nome = nome;
            this.preco = preco;
            this.quantidade = quantidade;
        }
        
        public String getNome() {
            return nome;
        }
        
        public double getPreco() {
            return preco;
        }
        
        public int getQuantidade() {
            return quantidade;
        }
        
        public void setQuantidade(int quantidade) {
            this.quantidade = quantidade;
        }
    }
    
    private static class Venda {
        private Produto produto;
        private int quantidade;
        private double valorTotal;
        
        public Venda(Produto produto, int quantidade, double valorTotal) {
            this.produto = produto;
            this.quantidade = quantidade;
            this.valorTotal = valorTotal;
        }
        
        public Produto getProduto() {
            return produto;
        }
        
        public int getQuantidade() {
            return quantidade;
        }
        
        public double getValorTotal() {
            return valorTotal;
        }
    }
}
```

Neste código, criamos um sistema de gerenciamento de vendas simples. O programa solicita ao usuário que escolha uma opção no menu, que inclui cadastrar um produto, realizar uma venda, exibir o estoque ou exibir as vendas realizadas. 

O código utiliza classes internas para representar os produtos e as vendas. A classe Produto possui atributos como nome, preço e quantidade em estoque, bem como métodos para acessar esses atributos. A classe Venda representa uma venda realizada, com referência ao produto vendido, quantidade e valor total da venda.

O programa também utiliza listas para armazenar os produtos em estoque e as vendas realizadas. Os métodos `cadastrarProduto`, `realizarVenda`, `exibirEstoque` e `exibirVendas` são responsáveis por interagir com as listas e realizar as ações correspondentes às opções escolhidas pelo usuário.

Espero que este exemplo atenda às suas expectativas! Lembre-se de que você pode modificar ou adaptar o código de acordo com suas necessidades.