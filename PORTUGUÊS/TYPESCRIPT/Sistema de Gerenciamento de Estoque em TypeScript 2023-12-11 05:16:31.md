Claro! Aqui está um código complexo em TypeScript que usa classes e interfaces para simular um sistema de gerenciamento de estoque de uma loja:

```typescript
interface IProduto {
  codigo: number;
  nome: string;
  quantidade: number;
  preco: number;
}

class Estoque {
  private produtos: IProduto[];

  constructor() {
    this.produtos = [];
  }

  adicionarProduto(produto: IProduto): void {
    const produtoExistente = this.produtos.find(p => p.codigo === produto.codigo);

    if (produtoExistente) {
      produtoExistente.quantidade += produto.quantidade;
    } else {
      this.produtos.push(produto);
    }
  }

  removerProduto(codigo: number, quantidade: number): void {
    const produtoExistente = this.produtos.find(p => p.codigo === codigo);

    if (produtoExistente) {
      if (produtoExistente.quantidade >= quantidade) {
        produtoExistente.quantidade -= quantidade;
        if (produtoExistente.quantidade === 0) {
          const index = this.produtos.indexOf(produtoExistente);
          this.produtos.splice(index, 1);
        }
      } else {
        console.log("Quantidade insuficiente em estoque.");
      }
    } else {
      console.log("Produto não encontrado.");
    }
  }

  listarProdutos(): void {
    this.produtos.forEach(produto => {
      console.log(`
        Código: ${produto.codigo}
        Nome: ${produto.nome}
        Quantidade: ${produto.quantidade}
        Preço: R$ ${produto.preco}
      `);
    });
  }
}

const estoque = new Estoque();

const produto1: IProduto = {
  codigo: 1,
  nome: "Camiseta",
  quantidade: 10,
  preco: 29.99
};

const produto2: IProduto = {
  codigo: 2,
  nome: "Calça",
  quantidade: 5,
  preco: 59.99
};

const produto3: IProduto = {
  codigo: 3,
  nome: "Tênis",
  quantidade: 3,
  preco: 99.99
};

estoque.adicionarProduto(produto1);
estoque.adicionarProduto(produto2);
estoque.adicionarProduto(produto3);

estoque.listarProdutos();

estoque.removerProduto(1, 2);

estoque.listarProdutos();
```

Neste código, temos a interface `IProduto` que define a estrutura de um produto, com as propriedades de código, nome, quantidade e preço. A classe `Estoque` representa o sistema de gerenciamento de estoque e possui métodos para adicionar e remover produtos, além de listar todos os produtos disponíveis.

No exemplo, criamos uma instância da classe `Estoque` e adicionamos três produtos. Em seguida, listamos os produtos e removemos 2 unidades do produto com código 1. Por fim, listamos novamente os produtos para visualizar as alterações no estoque.

Espero que este código complexo em TypeScript atenda às suas expectativas!