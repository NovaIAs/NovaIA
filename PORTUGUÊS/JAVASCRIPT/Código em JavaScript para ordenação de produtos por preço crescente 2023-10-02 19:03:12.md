Claro! Aqui está um código complexo em JavaScript que envolve manipulação de arrays, funções, objetos e algoritmos de ordenação:

```javascript
// Função para criar um objeto com informações de um produto
function criarProduto(nome, preco) {
  return {
    nome,
    preco
  };
}

// Array de objetos de produtos
const produtos = [
  criarProduto("Computador", 2500),
  criarProduto("Smartphone", 1500),
  criarProduto("TV", 2000),
  criarProduto("Notebook", 3000),
  criarProduto("Tablet", 1000)
];

// Função para ordenar os produtos por preço crescente
function ordenarProdutosPorPreco(produtos) {
  for (let i = 0; i < produtos.length - 1; i++) {
    for (let j = 0; j < produtos.length - i - 1; j++) {
      if (produtos[j].preco > produtos[j + 1].preco) {
        // Troca os produtos de posição
        const temp = produtos[j];
        produtos[j] = produtos[j + 1];
        produtos[j + 1] = temp;
      }
    }
  }
  return produtos;
}

// Ordena os produtos por preço crescente
const produtosOrdenados = ordenarProdutosPorPreco(produtos);

// Imprime os produtos ordenados
for (const produto of produtosOrdenados) {
  console.log(`Nome: ${produto.nome}, Preço: R$ ${produto.preco}`);
}
```

Este código cria uma função `criarProduto` para criar objetos de produtos com nome e preço. Em seguida, um array de objetos de produtos é criado e preenchido com alguns exemplos.

A função `ordenarProdutosPorPreco` implementa o algoritmo de ordenação conhecido como bubble sort para ordenar os produtos por preço crescente.

Por fim, o código chama a função `ordenarProdutosPorPreco` passando o array de produtos e imprime os produtos ordenados no console.