Claro! Vou gerar um código complexo em Java para você. Este código é um exemplo de um sistema de gerenciamento de estoque para uma loja, com algumas funcionalidades adicionais. Vou explicar o código linha por linha para facilitar o entendimento.

```java
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class GerenciamentoEstoque {

    private static List<Produto> estoque = new ArrayList<>();

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        boolean sair = false;

        while (!sair) {
            System.out.println("=== Gerenciamento de Estoque ===");
            System.out.println("1. Adicionar Produto");
            System.out.println("2. Remover Produto");
            System.out.println("3. Listar Estoque");
            System.out.println("4. Sair");
            System.out.print("Escolha uma opção: ");
            int opcao = scanner.nextInt();

            switch (opcao) {
                case 1:
                    adicionarProduto(scanner);
                    break;
                case 2:
                    removerProduto(scanner);
                    break;
                case 3:
                    listarEstoque();
                    break;
                case 4:
                    sair = true;
                    break;
                default:
                    System.out.println("Opção inválida!");
            }
        }

        System.out.println("Saindo do programa...");
    }

    private static void adicionarProduto(Scanner scanner) {
        System.out.println("=== Adicionar Produto ===");
        System.out.print("Nome do produto: ");
        String nome = scanner.next();
        System.out.print("Preço do produto: ");
        double preco = scanner.nextDouble();
        System.out.print("Quantidade em estoque: ");
        int quantidade = scanner.nextInt();

        Produto produto = new Produto(nome, preco, quantidade);
        estoque.add(produto);

        System.out.println("Produto adicionado com sucesso!");
    }

    private static void removerProduto(Scanner scanner) {
        System.out.println("=== Remover Produto ===");
        System.out.print("Nome do produto: ");
        String nome = scanner.next();

        Produto produtoRemovido = null;
        for (Produto produto : estoque) {
            if (produto.getNome().equals(nome)) {
                produtoRemovido = produto;
                break;
            }
        }

        if (produtoRemovido != null) {
            estoque.remove(produtoRemovido);
            System.out.println("Produto removido com sucesso!");
        } else {
            System.out.println("Produto não encontrado no estoque!");
        }
    }

    private static void listarEstoque() {
        System.out.println("=== Estoque ===");
        for (Produto produto : estoque) {
            System.out.println(produto);
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

        @Override
        public String toString() {
            return "Produto{" +
                    "nome='" + nome + '\'' +
                    ", preco=" + preco +
                    ", quantidade=" + quantidade +
                    '}';
        }
    }
}
```

Explicação do código:

1. Primeiro, importamos as classes `ArrayList`, `List` e `Scanner` para o código.

2. Em seguida, criamos a classe `GerenciamentoEstoque` que contém o método `main` e outras funções auxiliares.

3. Dentro do método `main`, criamos uma instância da classe `Scanner` para ler a entrada do usuário e inicializamos a variável `sair` como `false`.

4. Iniciamos um loop `while` que continuará executando até que o usuário escolha a opção "4. Sair".

5. Dentro do loop, exibimos um menu para o usuário escolher uma opção.

6. Utilizamos um `switch` para executar a função correspondente à opção escolhida pelo usuário. As opções são:

- 1: Adicionar um produto ao estoque, chamando o método `adicionarProduto`.
- 2: Remover um produto do estoque, chamando o método `removerProduto`.
- 3: Listar todos os produtos do estoque, chamando o método `listarEstoque`.
- 4: Sair do programa.

7. Foram definidas as funções `adicionarProduto`, `removerProduto` e `listarEstoque` para lidar com as respectivas funcionalidades.

8. Na função `adicionarProduto`, o usuário é solicitado a fornecer o nome, preço e quantidade do produto a ser adicionado. Em seguida, um objeto `Produto` é criado com base nas informações fornecidas e adicionado à lista `estoque`.

9. Na função `removerProduto`, o usuário é solicitado a fornecer o nome do produto a ser removido. Em seguida, um loop `for` é usado para procurar o produto na lista `estoque` e removê-lo, se encontrado.

10. Na função `listarEstoque`, todos os produtos presentes na lista `estoque` são exibidos na tela.

11. A classe interna `Produto` é usada para representar um produto no estoque. Ela possui atributos para o nome, preço e quantidade do produto, bem como métodos para acessar esses atributos.

12. O método `toString` é sobrescrito na classe `Produto` para exibir uma representação em formato de string do objeto.

Espero que este exemplo complexo de código em Java tenha atendido às suas expectativas!