Claro! Aqui está um exemplo de um código complexo em JavaScript que envolve a criação de uma loja virtual fictícia:

```javascript
// Definição da classe Produto
class Produto {
  constructor(nome, preco, quantidade) {
    this.nome = nome;
    this.preco = preco;
    this.quantidade = quantidade;
  }

  calcularTotal() {
    return this.preco * this.quantidade;
  }
}

// Definição da classe Carrinho
class Carrinho {
  constructor() {
    this.produtos = [];
  }

  adicionarProduto(produto) {
    this.produtos.push(produto);
  }

  calcularTotalCarrinho() {
    let total = 0;
    this.produtos.forEach((produto) => {
      total += produto.calcularTotal();
    });
    return total;
  }
}

// Criação de produtos
const produto1 = new Produto("Camiseta", 30, 2);
const produto2 = new Produto("Calça", 50, 1);
const produto3 = new Produto("Tênis", 100, 1);

// Criação do carrinho
const carrinho = new Carrinho();

// Adicionar produtos ao carrinho
carrinho.adicionarProduto(produto1);
carrinho.adicionarProduto(produto2);
carrinho.adicionarProduto(produto3);

// Calcular o total do carrinho
const totalCarrinho = carrinho.calcularTotalCarrinho();

console.log("Produtos no carrinho:");
carrinho.produtos.forEach((produto) => {
  console.log(`${produto.nome}: R$ ${produto.preco} x ${produto.quantidade}`);
});
console.log(`Total do carrinho: R$ ${totalCarrinho}`);
```

Neste código, temos a definição das classes `Produto` e `Carrinho`. A classe `Produto` representa um produto da loja, com propriedades como nome, preço e quantidade. O método `calcularTotal` calcula o valor total do produto, multiplicando o preço pela quantidade.

A classe `Carrinho` representa o carrinho de compras, com um array de produtos. O método `adicionarProduto` permite adicionar um produto ao carrinho. O método `calcularTotalCarrinho` calcula o valor total do carrinho, somando o valor total de cada produto.

Em seguida, são criados três produtos fictícios (`produto1`, `produto2` e `produto3`) e um carrinho (`carrinho`). Os produtos são adicionados ao carrinho usando o método `adicionarProduto`.

Por fim, o código exibe no console os produtos contidos no carrinho, com seus respectivos preços e quantidades, e o valor total do carrinho.