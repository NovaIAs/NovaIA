Claro! Aqui está um código complexo em Java que realiza uma simulação de um sistema de gerenciamento de pedidos em um restaurante. O código é dividido em várias classes para organizar melhor as funcionalidades. Vou explicar o código a cada parte.

```java
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

class ItemMenu {
    private String nome;
    private double preco;

    public ItemMenu(String nome, double preco) {
        this.nome = nome;
        this.preco = preco;
    }

    public String getNome() {
        return nome;
    }

    public double getPreco() {
        return preco;
    }
}

class Pedido {
    private int numero;
    private List<ItemMenu> itens;

    public Pedido(int numero) {
        this.numero = numero;
        this.itens = new ArrayList<>();
    }

    public int getNumero() {
        return numero;
    }

    public void adicionarItem(ItemMenu item) {
        itens.add(item);
    }

    public List<ItemMenu> getItens() {
        return itens;
    }

    public double calcularTotal() {
        double total = 0;
        for (ItemMenu item : itens) {
            total += item.getPreco();
        }
        return total;
    }
}

class Restaurante {
    private List<ItemMenu> menu;
    private List<Pedido> pedidos;
    private int numeroPedidoAtual;

    public Restaurante() {
        this.menu = new ArrayList<>();
        this.pedidos = new ArrayList<>();
        this.numeroPedidoAtual = 1;
    }

    public void adicionarItemAoMenu(ItemMenu item) {
        menu.add(item);
    }

    public void realizarPedido() {
        Pedido pedido = new Pedido(numeroPedidoAtual);
        numeroPedidoAtual++;

        Scanner scanner = new Scanner(System.in);
        int opcao = -1;

        while (opcao != 0) {
            System.out.println("Menu:");
            for (int i = 0; i < menu.size(); i++) {
                System.out.println((i + 1) + ". " + menu.get(i).getNome() + " - R$" + menu.get(i).getPreco());
            }
            System.out.println("0. Finalizar pedido");

            System.out.print("Escolha uma opção: ");
            opcao = scanner.nextInt();

            if (opcao > 0 && opcao <= menu.size()) {
                ItemMenu item = menu.get(opcao - 1);
                pedido.adicionarItem(item);
                System.out.println(item.getNome() + " adicionado ao pedido.");
            } else if (opcao != 0) {
                System.out.println("Opção inválida.");
            }
        }

        if (pedido.getItens().isEmpty()) {
            System.out.println("Pedido vazio. Nenhum item foi adicionado.");
        } else {
            pedidos.add(pedido);
            System.out.println("Pedido " + pedido.getNumero() + " realizado com sucesso.");
        }
    }

    public void exibirPedidos() {
        if (pedidos.isEmpty()) {
            System.out.println("Nenhum pedido realizado.");
        } else {
            for (Pedido pedido : pedidos) {
                System.out.println("Pedido " + pedido.getNumero() + ":");
                for (ItemMenu item : pedido.getItens()) {
                    System.out.println("- " + item.getNome() + " - R$" + item.getPreco());
                }
                System.out.println("Total: R$" + pedido.calcularTotal());
                System.out.println();
            }
        }
    }
}

public class Main {
    public static void main(String[] args) {
        Restaurante restaurante = new Restaurante();
        Scanner scanner = new Scanner(System.in);
        int opcao = -1;

        while (opcao != 0) {
            System.out.println("Restaurante");
            System.out.println("1. Adicionar item ao menu");
            System.out.println("2. Realizar pedido");
            System.out.println("3. Exibir pedidos");
            System.out.println("0. Sair");

            System.out.print("Escolha uma opção: ");
            opcao = scanner.nextInt();

            switch (opcao) {
                case 1:
                    System.out.print("Digite o nome do item: ");
                    String nomeItem = scanner.next();
                    System.out.print("Digite o preço do item: ");
                    double precoItem = scanner.nextDouble();
                    restaurante.adicionarItemAoMenu(new ItemMenu(nomeItem, precoItem));
                    System.out.println("Item adicionado ao menu.");
                    break;
                case 2:
                    restaurante.realizarPedido();
                    break;
                case 3:
                    restaurante.exibirPedidos();
                    break;
                case 0:
                    System.out.println("Saindo...");
                    break;
                default:
                    System.out.println("Opção inválida.");
                    break;
            }
        }
    }
}
```

Explicação do código:

1. A classe `ItemMenu` representa um item do menu do restaurante, contendo um nome e um preço.

2. A classe `Pedido` representa um pedido feito por um cliente, armazenando um número e uma lista de itens do menu.

3. A classe `Restaurante` é responsável por gerenciar o menu e os pedidos realizados. Ela possui métodos para adicionar itens ao menu, realizar pedidos e exibir os pedidos feitos.

4. A classe `Main` é a classe principal que contém o método `main`. Nela, é criada uma instância do restaurante e um loop que permite ao usuário interagir com o programa. São exibidas opções no menu e, dependendo da escolha, as funções correspondentes do restaurante são chamadas.

Esse código simula um sistema básico de pedidos em um restaurante, onde é possível adicionar itens ao menu, realizar pedidos e exibir os pedidos feitos. O código utiliza a classe `Scanner` para receber as entradas do usuário.