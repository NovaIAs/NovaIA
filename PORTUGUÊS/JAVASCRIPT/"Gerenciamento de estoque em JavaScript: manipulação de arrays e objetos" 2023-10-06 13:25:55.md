Claro, vou criar um código em JavaScript que envolve manipulação de arrays e objetos. O código abaixo simula um sistema de gerenciamento de estoque de uma loja:

```javascript
// Definindo a classe Produto
class Produto {
  constructor(nome, preco, quantidade) {
    this.nome = nome;
    this.preco = preco;
    this.quantidade = quantidade;
  }

  calcularValorTotal() {
    return this.preco * this.quantidade;
  }
}

// Criando alguns produtos
const produto1 = new Produto("Camiseta", 29.99, 10);
const produto2 = new Produto("Calça", 59.99, 5);
const produto3 = new Produto("Tênis", 99.99, 3);

// Adicionando os produtos ao estoque
const estoque = [];
estoque.push(produto1);
estoque.push(produto2);
estoque.push(produto3);

// Função para calcular o valor total do estoque
function calcularValorTotalEstoque() {
  let valorTotalEstoque = 0;

  estoque.forEach((produto) => {
    valorTotalEstoque += produto.calcularValorTotal();
  });

  return valorTotalEstoque;
}

// Função para encontrar um produto pelo nome
function encontrarProdutoPorNome(nome) {
  const produtoEncontrado = estoque.find((produto) => {
    return produto.nome === nome;
  });

  return produtoEncontrado;
}

// Atualizando a quantidade de um produto no estoque
function atualizarQuantidadeProduto(nome, novaQuantidade) {
  const produtoAtualizado = encontrarProdutoPorNome(nome);

  if (produtoAtualizado) {
    produtoAtualizado.quantidade = novaQuantidade;
    console.log(`A quantidade do ${nome} foi atualizada para ${novaQuantidade}.`);
  } else {
    console.log(`O produto ${nome} não está no estoque.`);
  }
}

// Removendo um produto do estoque
function removerProduto(nome) {
  const produtoRemovido = encontrarProdutoPorNome(nome);

  if (produtoRemovido) {
    const index = estoque.indexOf(produtoRemovido);
    estoque.splice(index, 1);
    console.log(`O produto ${nome} foi removido do estoque.`);
  } else {
    console.log(`O produto ${nome} não está no estoque.`);
  }
}

// Exemplo de utilização das funções
console.log("Valor total do estoque:", calcularValorTotalEstoque());

console.log(encontrarProdutoPorNome("Camiseta"));
console.log(encontrarProdutoPorNome("Meia"));

atualizarQuantidadeProduto("Calça", 8);
atualizarQuantidadeProduto("Boné", 2);

removerProduto("Tênis");
removerProduto("Chinelo");
```

Neste código, começamos definindo a classe `Produto`, que tem os atributos `nome`, `preco` e `quantidade`, e o método `calcularValorTotal()` para calcular o valor total do produto com base no preço e quantidade.

Em seguida, criamos alguns objetos `Produto` e os adicionamos ao array `estoque` usando o método `push()`.

Temos também as funções `calcularValorTotalEstoque()`, que percorre o array `estoque` e calcula o valor total de todos os produtos no estoque; `encontrarProdutoPorNome(nome)`, que encontra um produto no estoque com base no nome fornecido; `atualizarQuantidadeProduto(nome, novaQuantidade)`, que atualiza a quantidade de um produto no estoque; e `removerProduto(nome)`, que remove um produto do estoque.

Por fim, temos exemplos de utilização das funções, onde mostramos o valor total do estoque, encontramos produtos pelo nome, atualizamos a quantidade de um produto e removemos um produto do estoque.

Esse código demonstra algumas funcionalidades de um sistema de gerenciamento de estoque de uma loja. É um exemplo de código complexo em JavaScript que pode ser adaptado e expandido para atender diferentes necessidades.