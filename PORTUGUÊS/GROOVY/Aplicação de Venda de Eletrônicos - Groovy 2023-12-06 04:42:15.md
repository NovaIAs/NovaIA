Claro! Vou gerar um código complexo em Groovy para você. Tenha em mente que, embora eu seja capaz de gerar código, meu objetivo principal é fornecer assistência. Códigos extremamente complexos e exclusivos podem levar um tempo significativo para serem gerados. Aqui está um exemplo de código em Groovy para uma aplicação de venda de eletrônicos:

```groovy
import java.util.List;
import java.util.ArrayList;

class Produto {
    String nome
    double preco
    int quantidadeEmEstoque

    Produto(String nome, double preco, int quantidadeEmEstoque) {
        this.nome = nome
        this.preco = preco
        this.quantidadeEmEstoque = quantidadeEmEstoque
    }

    void exibirInformacoes() {
        println("Nome: $nome")
        println("Preço: R\$${preco}")
        println("Quantidade em estoque: $quantidadeEmEstoque")
    }
}

class Carrinho {
    List<Produto> produtos

    Carrinho() {
        produtos = new ArrayList<Produto>()
    }

    void adicionarProduto(Produto produto) {
        produtos.add(produto)
    }

    void removerProduto(Produto produto) {
        produtos.remove(produto)
    }

    double calcularTotal() {
        double total = 0.0
        for (Produto produto : produtos) {
            total += produto.preco
        }
        return total
    }

    void exibirProdutos() {
        for (Produto produto : produtos) {
            produto.exibirInformacoes()
            println("--------------------")
        }
    }
}

class VendaEletronicosApp {
    static void main(String[] args) {
        Produto notebook = new Produto("Notebook", 3000.0, 5)
        Produto smartphone = new Produto("Smartphone", 1500.0, 10)
        Produto tv = new Produto("TV", 2000.0, 8)

        Carrinho carrinho = new Carrinho()
        carrinho.adicionarProduto(notebook)
        carrinho.adicionarProduto(smartphone)
        carrinho.adicionarProduto(tv)

        carrinho.exibirProdutos()
        println("Total da compra: R\$${carrinho.calcularTotal()}")

        carrinho.removerProduto(notebook)

        carrinho.exibirProdutos()
        println("Total da compra: R\$${carrinho.calcularTotal()}")
    }
}
```

Nesse exemplo, criamos três classes: `Produto`, `Carrinho` e `VendaEletronicosApp`. A classe `Produto` representa um objeto de produto, com nome, preço e quantidade em estoque.

A classe `Carrinho` representa o carrinho de compras, que possui uma lista de produtos. Ela permite adicionar e remover produtos do carrinho, calcular o total da compra e exibir os produtos presentes no carrinho.

Por fim, a classe `VendaEletronicosApp` é a classe principal que executa a aplicação. Nela, criamos instâncias dos produtos, adicionamos os produtos ao carrinho, exibimos os produtos e o total da compra, removemos um produto do carrinho e exibimos novamente os produtos e o novo total da compra.

Espero que esse código complexo tenha sido do seu interesse! Caso precise de qualquer outro código ou tenha alguma dúvida, estou aqui para ajudar.