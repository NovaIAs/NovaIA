Claro! Aqui está um código complexo em Java que utiliza várias funcionalidades da linguagem para criar um programa de simulação de uma loja de videogames. O código está em português e possui comentários explicativos para facilitar o entendimento:

```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class LojaDeVideoGames {
    // Classe que representa um videogame
    static class VideoGame {
        private String nome;
        private String console;
        private int anoLancamento;
        private double preco;

        public VideoGame(String nome, String console, int anoLancamento, double preco) {
            this.nome = nome;
            this.console = console;
            this.anoLancamento = anoLancamento;
            this.preco = preco;
        }

        public String getNome() {
            return nome;
        }

        public String getConsole() {
            return console;
        }

        public int getAnoLancamento() {
            return anoLancamento;
        }

        public double getPreco() {
            return preco;
        }

        @Override
        public String toString() {
            return nome + " - " + console + " (" + anoLancamento + ") - R$" + preco;
        }
    }

    // Classe que representa a loja de videogames
    static class Loja {
        private List<VideoGame> videoGames;
        private Map<String, Integer> estoque;

        public Loja() {
            this.videoGames = new ArrayList<>();
            this.estoque = new HashMap<>();
        }

        public void adicionarVideoGame(VideoGame videoGame, int quantidade) {
            videoGames.add(videoGame);
            estoque.put(videoGame.getNome(), quantidade);
        }

        public void listarVideoGames() {
            System.out.println("Lista de Video Games:");
            for (VideoGame videoGame : videoGames) {
                System.out.println(videoGame);
            }
        }

        public void listarEstoque() {
            System.out.println("Estoque de Video Games:");
            for (Map.Entry<String, Integer> entry : estoque.entrySet()) {
                String nomeVideoGame = entry.getKey();
                int quantidade = entry.getValue();
                System.out.println(nomeVideoGame + " - Quantidade: " + quantidade);
            }
        }

        public void comprarVideoGame(String nome, int quantidade) {
            if (estoque.containsKey(nome)) {
                int estoqueAtual = estoque.get(nome);
                if (estoqueAtual >= quantidade) {
                    estoque.put(nome, estoqueAtual - quantidade);
                    System.out.println(quantidade + " " + nome + "(s) comprado(s) com sucesso!");
                } else {
                    System.out.println("Quantidade insuficiente em estoque!");
                }
            } else {
                System.out.println("Video Game não encontrado!");
            }
        }
    }

    public static void main(String[] args) {
        Loja loja = new Loja();

        // Adicionando video games ao estoque
        loja.adicionarVideoGame(new VideoGame("The Last of Us Part II", "PlayStation 4", 2020, 199.99), 10);
        loja.adicionarVideoGame(new VideoGame("Super Mario Odyssey", "Nintendo Switch", 2017, 299.99), 5);
        loja.adicionarVideoGame(new VideoGame("Gears 5", "Xbox One", 2019, 149.99), 8);

        Scanner scanner = new Scanner(System.in);

        boolean sair = false;
        while (!sair) {
            System.out.println("--- LOJA DE VIDEOGAMES ---");
            System.out.println("1 - Listar Video Games");
            System.out.println("2 - Listar Estoque");
            System.out.println("3 - Comprar Video Game");
            System.out.println("4 - Sair");
            System.out.print("Escolha uma opção: ");
            int opcao = scanner.nextInt();
            scanner.nextLine();

            switch (opcao) {
                case 1:
                    loja.listarVideoGames();
                    break;
                case 2:
                    loja.listarEstoque();
                    break;
                case 3:
                    System.out.print("Digite o nome do Video Game: ");
                    String nomeVideoGame = scanner.nextLine();
                    System.out.print("Digite a quantidade desejada: ");
                    int quantidade = scanner.nextInt();
                    loja.comprarVideoGame(nomeVideoGame, quantidade);
                    break;
                case 4:
                    sair = true;
                    break;
                default:
                    System.out.println("Opção inválida!");
                    break;
            }
            System.out.println();
        }

        System.out.println("Obrigado por utilizar a Loja de Video Games!");
        scanner.close();
    }
}
```

Este código implementa uma estrutura de classes para representar uma loja de videogames, permitindo listar os videogames disponíveis, verificar o estoque e comprar os videogames desejados. A interação com o usuário é feita através de um menu exibido no console.

A classe principal `LojaDeVideoGames` possui duas classes internas: `VideoGame`, que representa um videogame com nome, console, ano de lançamento e preço; e `Loja`, que contém a lista de videogames e o estoque.

A loja é inicializada com alguns videogames adicionados ao estoque. Em seguida, um loop principal é executado, exibindo o menu de opções para o usuário. As opções são: listar videogames, listar estoque, comprar videogame e sair. Dependendo da opção escolhida, o usuário é solicitado a inserir informações adicionais, como o nome do videogame desejado e a quantidade a ser comprada.

Espero que este código atenda às suas expectativas!