Claro! Vou gerar um código complexo em PHP para você. Este código é um exemplo de um sistema de gerenciamento de vendas para uma loja fictícia. Ele possui funcionalidades como cadastro de produtos, registro de vendas, cálculo de total de vendas, entre outros recursos. Vamos lá:

```php
<?php

// Classe Produto
class Produto {
    private $nome;
    private $preco;

    public function __construct($nome, $preco) {
        $this->nome = $nome;
        $this->preco = $preco;
    }

    public function getNome() {
        return $this->nome;
    }

    public function getPreco() {
        return $this->preco;
    }
}

// Classe Venda
class Venda {
    private $produtos;
    private $total;

    public function __construct() {
        $this->produtos = [];
        $this->total = 0;
    }

    public function adicionarProduto(Produto $produto) {
        $this->produtos[] = $produto;
        $this->total += $produto->getPreco();
    }

    public function getTotal() {
        return $this->total;
    }

    public function exibirProdutos() {
        foreach ($this->produtos as $produto) {
            echo "Nome: " . $produto->getNome() . ", Preço: " . $produto->getPreco() . "<br>";
        }
    }
}

// Utilização do sistema
$venda = new Venda();

$produto1 = new Produto("Camiseta", 29.99);
$produto2 = new Produto("Calça Jeans", 79.99);
$produto3 = new Produto("Tênis", 99.99);

$venda->adicionarProduto($produto1);
$venda->adicionarProduto($produto2);
$venda->adicionarProduto($produto3);

echo "Produtos da Venda:<br>";
$venda->exibirProdutos();

echo "<br>Total da Venda: " . $venda->getTotal();

?>
```

Neste código, temos duas classes principais: `Produto` e `Venda`. A classe `Produto` representa um produto da loja, com atributos como nome e preço. A classe `Venda` representa uma venda, com um array de produtos e um total.

No exemplo, criamos uma instância da classe `Venda` e três instâncias da classe `Produto`. Em seguida, adicionamos os produtos à venda e exibimos os produtos e o total da venda.

Espero que esse código complexo atenda às suas expectativas!