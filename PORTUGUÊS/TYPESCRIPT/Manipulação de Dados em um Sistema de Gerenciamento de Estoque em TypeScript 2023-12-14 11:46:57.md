Certamente! Aqui está um exemplo de código complexo em TypeScript que pode ser útil para trabalhar com manipulação de dados em um sistema de gerenciamento de estoque:

```typescript
// Definição da classe Produto
class Produto {
  private codigo: number;
  private nome: string;
  private quantidade: number;
  
  constructor(codigo: number, nome: string, quantidade: number) {
    this.codigo = codigo;
    this.nome = nome;
    this.quantidade = quantidade;
  }
  
  // Métodos para acessar e modificar os atributos privados
  public getCodigo(): number {
    return this.codigo;
  }
  
  public setCodigo(codigo: number): void {
    this.codigo = codigo;
  }
  
  public getNome(): string {
    return this.nome;
  }
  
  public setNome(nome: string): void {
    this.nome = nome;
  }
  
  public getQuantidade(): number {
    return this.quantidade;
  }
  
  public setQuantidade(quantidade: number): void {
    this.quantidade = quantidade;
  }
  
  // Método para exibir os detalhes do produto
  public exibirDetalhes(): void {
    console.log(`Código: ${this.codigo}`);
    console.log(`Nome: ${this.nome}`);
    console.log(`Quantidade: ${this.quantidade}`);
  }
}

// Definição da classe Estoque
class Estoque {
  private produtos: Produto[];

  constructor() {
    this.produtos = [];
  }
  
  // Método para adicionar um produto ao estoque
  public adicionarProduto(produto: Produto): void {
    this.produtos.push(produto);
  }
  
  // Método para remover um produto do estoque
  public removerProduto(codigo: number): void {
    const indice = this.produtos.findIndex(produto => produto.getCodigo() === codigo);
    if (indice !== -1) {
      this.produtos.splice(indice, 1);
    }
  }
  
  // Método para exibir todos os produtos no estoque
  public exibirProdutos(): void {
    this.produtos.forEach(produto => produto.exibirDetalhes());
  }
}

// Exemplo de utilização das classes
const estoque = new Estoque();
const produto1 = new Produto(1, "Camiseta", 10);
const produto2 = new Produto(2, "Calça", 5);
const produto3 = new Produto(3, "Sapato", 3);

estoque.adicionarProduto(produto1);
estoque.adicionarProduto(produto2);
estoque.adicionarProduto(produto3);

estoque.exibirProdutos();

estoque.removerProduto(2);

estoque.exibirProdutos();
```

Neste código, temos a definição das classes `Produto` e `Estoque`. A classe `Produto` possui atributos privados (`codigo`, `nome` e `quantidade`) e métodos para acessar e modificar esses atributos. Além disso, ela possui um método `exibirDetalhes()` que imprime no console os detalhes do produto.

A classe `Estoque` possui um atributo privado `produtos` que armazena um array de objetos do tipo `Produto`. Ela possui métodos para adicionar produtos ao estoque, remover produtos do estoque e exibir todos os produtos no estoque.

No exemplo de utilização das classes, criamos uma instância da classe `Estoque` e três instâncias da classe `Produto`. Em seguida, adicionamos os produtos ao estoque, exibimos todos os produtos e removemos um produto do estoque. Por fim, exibimos novamente todos os produtos no estoque.

Espero que esse exemplo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição.