Claro! Vou criar um código complexo em TypeScript. Farei um exemplo de um sistema de gerenciamento de estoque de uma loja fictícia. Vou criar classes para representar os produtos, o estoque e também para realizar as operações de adicionar, remover e listar produtos do estoque. 

Primeiramente, vamos criar a classe Produto, que representará um produto da loja. Cada produto terá um nome, uma descrição, um preço e uma quantidade em estoque. Faremos uso de getters e setters para garantir o encapsulamento dos atributos. Aqui está o código:

```typescript
class Produto {
  private nome: string;
  private descricao: string;
  private preco: number;
  private quantidade: number;

  constructor(nome: string, descricao: string, preco: number, quantidade: number) {
    this.nome = nome;
    this.descricao = descricao;
    this.preco = preco;
    this.quantidade = quantidade;
  }

  // Getters e Setters
  getNome(): string {
    return this.nome;
  }

  setNome(nome: string): void {
    this.nome = nome;
  }

  getDescricao(): string {
    return this.descricao;
  }

  setDescricao(descricao: string): void {
    this.descricao = descricao;
  }

  getPreco(): number {
    return this.preco;
  }

  setPreco(preco: number): void {
    this.preco = preco;
  }

  getQuantidade(): number {
    return this.quantidade;
  }

  setQuantidade(quantidade: number): void {
    this.quantidade = quantidade;
  }
}
```

Em seguida, vamos criar a classe Estoque, responsável por gerenciar os produtos em estoque. A classe Estoque terá um array de objetos do tipo Produto. Ela terá métodos para adicionar um produto ao estoque, remover um produto do estoque e listar todos os produtos disponíveis no estoque. Aqui está o código:

```typescript
class Estoque {
  private produtos: Produto[];

  constructor() {
    this.produtos = [];
  }

  adicionarProduto(produto: Produto): void {
    this.produtos.push(produto);
  }

  removerProduto(produto: Produto): void {
    const index = this.produtos.indexOf(produto);
    if (index !== -1) {
      this.produtos.splice(index, 1);
    }
  }

  listarProdutos(): void {
    console.log("Produtos em estoque:");
    for (let produto of this.produtos) {
      console.log(`${produto.getNome()} - ${produto.getDescricao()}, R$${produto.getPreco()}, Quantidade: ${produto.getQuantidade()}`);
    }
  }
}
```

Finalmente, vamos utilizar as classes Produto e Estoque para realizar as operações no estoque. Vamos criar alguns objetos Produto, adicioná-los ao estoque, remover um deles e listar todos os produtos disponíveis no estoque. Aqui está o código:

```typescript
// Criando produtos
const produto1 = new Produto("Camiseta", "Camiseta de algodão", 29.99, 50);
const produto2 = new Produto("Calça", "Calça jeans", 79.99, 30);
const produto3 = new Produto("Boné", "Boné de brim", 19.99, 20);

// Criando estoque
const estoque = new Estoque();

// Adicionando produtos ao estoque
estoque.adicionarProduto(produto1);
estoque.adicionarProduto(produto2);
estoque.adicionarProduto(produto3);

// Listando produtos em estoque
estoque.listarProdutos();

// Removendo um produto do estoque
estoque.removerProduto(produto2);

// Listando produtos em estoque novamente
estoque.listarProdutos();
```

Esse é um exemplo de código complexo em TypeScript que simula um sistema de gerenciamento de estoque de uma loja. Utilizando as classes Produto e Estoque, é possível adicionar, remover e listar produtos do estoque de forma organizada. Espero que isso te ajude a entender a estruturação e funcionalidades em TypeScript.