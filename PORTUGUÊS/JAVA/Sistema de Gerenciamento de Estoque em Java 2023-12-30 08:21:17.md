Claro! Aqui está um código complexo em Java que envolve a criação de um sistema de gerenciamento de estoque para uma loja fictícia. O código utiliza conceitos de orientação a objetos, estruturas de dados e manipulação de arquivos.

```java
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class GerenciamentoEstoque {
    
    private static final String NOME_ARQUIVO = "estoque.txt";
    private static List<Produto> estoque;
    
    public static void main(String[] args) {
        estoque = new ArrayList<>();
        carregarEstoque();
        
        Scanner scanner = new Scanner(System.in);
        int opcao;
        
        do {
            System.out.println("=== Gerenciamento de Estoque ===");
            System.out.println("1. Listar produtos");
            System.out.println("2. Adicionar produto");
            System.out.println("3. Remover produto");
            System.out.println("4. Atualizar quantidade");
            System.out.println("0. Sair");
            System.out.print("Escolha uma opção: ");
            
            opcao = scanner.nextInt();
            
            switch (opcao) {
                case 1:
                    listarProdutos();
                    break;
                case 2:
                    adicionarProduto(scanner);
                    break;
                case 3:
                    removerProduto(scanner);
                    break;
                case 4:
                    atualizarQuantidade(scanner);
                    break;
                case 0:
                    System.out.println("Encerrando o programa...");
                    break;
                default:
                    System.out.println("Opção inválida!");
                    break;
            }
        } while (opcao != 0);
        
        salvarEstoque();
        scanner.close();
    }
    
    private static void carregarEstoque() {
        try (BufferedReader br = new BufferedReader(new FileReader(NOME_ARQUIVO))) {
            String linha;
            while ((linha = br.readLine()) != null) {
                String[] dados = linha.split(";");
                String nome = dados[0];
                double preco = Double.parseDouble(dados[1]);
                int quantidade = Integer.parseInt(dados[2]);
                Produto produto = new Produto(nome, preco, quantidade);
                estoque.add(produto);
            }
            System.out.println("Estoque carregado com sucesso!");
        } catch (IOException e) {
            System.out.println("Erro ao carregar o estoque!");
        }
    }
    
    private static void salvarEstoque() {
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(NOME_ARQUIVO))) {
            for (Produto produto : estoque) {
                String linha = produto.getNome() + ";" + produto.getPreco() + ";" + produto.getQuantidade();
                bw.write(linha);
                bw.newLine();
            }
            System.out.println("Estoque salvo com sucesso!");
        } catch (IOException e) {
            System.out.println("Erro ao salvar o estoque!");
        }
    }
    
    private static void listarProdutos() {
        if (estoque.isEmpty()) {
            System.out.println("Estoque vazio!");
        } else {
            System.out.println("=== Produtos no estoque ===");
            for (Produto produto : estoque) {
                System.out.println(produto);
            }
        }
    }
    
    private static void adicionarProduto(Scanner scanner) {
        scanner.nextLine();
        
        System.out.print("Digite o nome do produto: ");
        String nome = scanner.nextLine();
        
        System.out.print("Digite o preço do produto: ");
        double preco = scanner.nextDouble();
        
        System.out.print("Digite a quantidade do produto: ");
        int quantidade = scanner.nextInt();
        
        Produto produto = new Produto(nome, preco, quantidade);
        estoque.add(produto);
        System.out.println("Produto adicionado com sucesso!");
    }
    
    private static void removerProduto(Scanner scanner) {
        scanner.nextLine();
        
        System.out.print("Digite o nome do produto a ser removido: ");
        String nome = scanner.nextLine();
        
        boolean removido = false;
        for (int i = 0; i < estoque.size(); i++) {
            Produto produto = estoque.get(i);
            if (produto.getNome().equals(nome)) {
                estoque.remove(i);
                removido = true;
                break;
            }
        }
        
        if (removido) {
            System.out.println("Produto removido com sucesso!");
        } else {
            System.out.println("Produto não encontrado!");
        }
    }
    
    private static void atualizarQuantidade(Scanner scanner) {
        scanner.nextLine();
        
        System.out.print("Digite o nome do produto a ser atualizado: ");
        String nome = scanner.nextLine();
        
        System.out.print("Digite a nova quantidade do produto: ");
        int novaQuantidade = scanner.nextInt();
        
        boolean atualizado = false;
        for (Produto produto : estoque) {
            if (produto.getNome().equals(nome)) {
                produto.setQuantidade(novaQuantidade);
                atualizado = true;
                break;
            }
        }
        
        if (atualizado) {
            System.out.println("Quantidade atualizada com sucesso!");
        } else {
            System.out.println("Produto não encontrado!");
        }
    }
}

class Produto {
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
    
    @Override
    public String toString() {
        return "Produto [nome=" + nome + ", preco=" + preco + ", quantidade=" + quantidade + "]";
    }
}
```

Este código cria um programa de gerenciamento de estoque que permite ao usuário realizar várias operações, como listar produtos, adicionar produtos, remover produtos e atualizar a quantidade de produtos existentes no estoque.

O programa utiliza um arquivo chamado "estoque.txt" para armazenar os dados do estoque. O arquivo é carregado ao iniciar o programa e salvo novamente ao encerrá-lo.

O programa utiliza uma classe chamada `Produto` para representar cada produto do estoque. Cada produto possui um nome, um preço e uma quantidade.

No método `carregarEstoque()`, o programa lê cada linha do arquivo "estoque.txt" e cria um objeto `Produto` para cada linha, adicionando-o à lista `estoque`.

No método `salvarEstoque()`, o programa percorre a lista `estoque` e escreve os dados de cada produto no arquivo "estoque.txt", separando-os por ponto e vírgula (;).

No método `listarProdutos()`, o programa exibe na tela todos os produtos presentes no estoque.

No método `adicionarProduto()`, o programa solicita ao usuário que digite o nome, o preço e a quantidade do novo produto a ser adicionado. Em seguida, cria um novo objeto `Produto` com os dados informados e o adiciona à lista `estoque`.

No método `removerProduto()`, o programa solicita ao usuário que digite o nome do produto a ser removido. Em seguida, percorre a lista `estoque` e remove o objeto `Produto` encontrado com o mesmo nome.

No método `atualizarQuantidade()`, o programa solicita ao usuário que digite o nome do produto a ser atualizado e a nova quantidade. Em seguida, percorre a lista `estoque` e atualiza a quantidade do objeto `Produto` encontrado com o mesmo nome.

Ao final, o programa salva as alterações no estoque e encerra a execução.