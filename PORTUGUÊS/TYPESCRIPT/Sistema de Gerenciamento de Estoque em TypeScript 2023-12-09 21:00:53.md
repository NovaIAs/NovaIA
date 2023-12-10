Claro, vou criar um exemplo de código complexo em TypeScript para você. Este código simulará um sistema de gerenciamento de estoque para uma loja fictícia. Será um exemplo completo com várias classes e métodos. Vamos lá:

```typescript
// Classe para representar um produto
class Produto {
  nome: string;
  preco: number;
  quantidade: number;

  constructor(nome: string, preco: number, quantidade: number) {
    this.nome = nome;
    this.preco = preco;
    this.quantidade = quantidade;
  }

  calcularValorTotal(): number {
    return this.preco * this.quantidade;
  }
}

// Classe para representar o estoque da loja
class Estoque {
  produtos: Produto[];

  constructor() {
    this.produtos = [];
  }

  adicionarProduto(produto: Produto): void {
    this.produtos.push(produto);
  }

  removerProduto(nome: string): void {
    this.produtos = this.produtos.filter((produto) => produto.nome !== nome);
  }

  buscarProduto(nome: string): Produto | undefined {
    return this.produtos.find((produto) => produto.nome === nome);
  }

  listarProdutos(): void {
    this.produtos.forEach((produto) => {
      console.log(`Nome: ${produto.nome}`);
      console.log(`Preço: R$${produto.preco}`);
      console.log(`Quantidade: ${produto.quantidade}`);
      console.log(`Valor Total: R$${produto.calcularValorTotal()}`);
      console.log('------------------');
    });
  }
}

// Utilização do código

// Criando instância do estoque
const estoque = new Estoque();

// Adicionando produtos ao estoque
estoque.adicionarProduto(new Produto('Camiseta', 49.99, 10));
estoque.adicionarProduto(new Produto('Calça', 79.99, 5));
estoque.adicionarProduto(new Produto('Tênis', 149.99, 3));

// Listando todos os produtos no estoque
estoque.listarProdutos();

// Buscando um produto específico
const produto = estoque.buscarProduto('Camiseta');
if (produto) {
  console.log('Produto encontrado:');
  console.log(`Nome: ${produto.nome}`);
  console.log(`Preço: R$${produto.preco}`);
  console.log(`Quantidade: ${produto.quantidade}`);
  console.log(`Valor Total: R$${produto.calcularValorTotal()}`);
} else {
  console.log('Produto não encontrado.');
}

// Removendo um produto do estoque
estoque.removerProduto('Calça');

// Listando novamente os produtos no estoque após a remoção
estoque.listarProdutos();
```

Neste exemplo, criamos duas classes: `Produto` e `Estoque`. A classe `Produto` representa um produto com nome, preço e quantidade. A classe `Estoque` representa o estoque da loja e possui métodos para adicionar, remover, buscar e listar produtos.

No código de exemplo, criamos uma instância do estoque, adicionamos alguns produtos, listamos todos os produtos, buscamos um produto específico, removemos um produto e listamos novamente os produtos após a remoção.

Espero que este exemplo te ajude a entender como utilizar TypeScript em um sistema de gerenciamento de estoque. Este é apenas um exemplo, mas com base nele você pode criar um sistema mais complexo e adaptá-lo às suas necessidades.