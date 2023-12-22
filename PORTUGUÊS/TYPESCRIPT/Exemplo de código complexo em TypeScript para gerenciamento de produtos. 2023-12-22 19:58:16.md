Claro! Aqui está um exemplo de código complexo em TypeScript que utiliza classes e interfaces para criar uma aplicação de gerenciamento de produtos.

```typescript
interface Produto {
  id: number;
  nome: string;
  preco: number;
}

class ProdutoService {
  private produtos: Produto[];
  
  constructor() {
    this.produtos = [];
  }
  
  adicionarProduto(produto: Produto): void {
    this.produtos.push(produto);
  }
  
  removerProduto(id: number): void {
    const index = this.produtos.findIndex((produto) => produto.id === id);
    if (index !== -1) {
      this.produtos.splice(index, 1);
    }
  }
  
  listarProdutos(): Produto[] {
    return this.produtos;
  }
  
  buscarProduto(id: number): Produto | undefined {
    return this.produtos.find((produto) => produto.id === id);
  }
}

class Aplicacao {
  private produtoService: ProdutoService;
  
  constructor() {
    this.produtoService = new ProdutoService();
  }
  
  adicionarProduto(): void {
    const id = Number(prompt("Digite o ID do produto:"));
    const nome = prompt("Digite o nome do produto:");
    const preco = Number(prompt("Digite o preço do produto:"));
    
    const produto: Produto = { id, nome, preco };
    
    this.produtoService.adicionarProduto(produto);
    
    console.log("Produto adicionado com sucesso!");
  }
  
  removerProduto(): void {
    const id = Number(prompt("Digite o ID do produto que deseja remover:"));
    
    this.produtoService.removerProduto(id);
    
    console.log("Produto removido com sucesso!");
  }
  
  listarProdutos(): void {
    const produtos = this.produtoService.listarProdutos();
    
    if (produtos.length === 0) {
      console.log("Nenhum produto encontrado.");
    } else {
      console.log("Lista de produtos:");
      produtos.forEach((produto) => {
        console.log(`ID: ${produto.id}, Nome: ${produto.nome}, Preço: R$${produto.preco}`);
      });
    }
  }
  
  buscarProduto(): void {
    const id = Number(prompt("Digite o ID do produto que deseja buscar:"));
    
    const produto = this.produtoService.buscarProduto(id);
    
    if (produto) {
      console.log(`Produto encontrado: ID: ${produto.id}, Nome: ${produto.nome}, Preço: R$${produto.preco}`);
    } else {
      console.log("Produto não encontrado.");
    }
  }
}

const aplicacao = new Aplicacao();

while (true) {
  const opcao = Number(prompt(
    "Digite uma opção:\n" +
    "1 - Adicionar produto\n" +
    "2 - Remover produto\n" +
    "3 - Listar produtos\n" +
    "4 - Buscar produto\n" +
    "5 - Sair"
  ));
  
  switch (opcao) {
    case 1:
      aplicacao.adicionarProduto();
      break;
    case 2:
      aplicacao.removerProduto();
      break;
    case 3:
      aplicacao.listarProdutos();
      break;
    case 4:
      aplicacao.buscarProduto();
      break;
    case 5:
      console.log("Saindo...");
      process.exit(0);
    default:
      console.log("Opção inválida.");
  }
}
```

Neste exemplo, temos uma classe `ProdutoService` que é responsável por manipular os produtos (adicionar, remover, listar e buscar). A classe `Aplicacao` utiliza a instância de `ProdutoService` para interagir com os produtos através de um menu de opções em um loop infinito.

É importante destacar o uso de interfaces para definir a estrutura do objeto `Produto` e tornar o código mais legível e reutilizável. Além disso, o código utiliza a função `prompt` para obter a entrada do usuário e exibe mensagens no console para comunicar as ações realizadas.