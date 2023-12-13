Claro! Aqui está um código complexo em TypeScript que utiliza classes, herança, interfaces e o padrão de projeto Singleton para a criação de um sistema de controle de vendas:

```typescript
interface IProduto {
  nome: string;
  preco: number;
  calcularDesconto(): number;
}

class Produto implements IProduto {
  constructor(public nome: string, public preco: number) {}

  calcularDesconto() {
    return this.preco * 0.1; // 10% de desconto
  }
}

class ProdutoImportado extends Produto {
  constructor(nome: string, preco: number, private taxaImportacao: number) {
    super(nome, preco);
  }

  calcularDesconto() {
    const desconto = super.calcularDesconto();
    return desconto - this.taxaImportacao;
  }
}

class Venda {
  private static instancia: Venda;
  private produtos: IProduto[] = [];

  private constructor() {}

  public static getInstance(): Venda {
    if (!Venda.instancia) {
      Venda.instancia = new Venda();
    }
    return Venda.instancia;
  }

  adicionarProduto(produto: IProduto) {
    this.produtos.push(produto);
  }

  calcularTotal(): number {
    let total = 0;
    for (const produto of this.produtos) {
      total += produto.preco - produto.calcularDesconto();
    }
    return total;
  }
}

// Exemplo de uso
const venda = Venda.getInstance();
const produto1 = new Produto("Camiseta", 50);
const produto2 = new ProdutoImportado("Celular", 1000, 100);

venda.adicionarProduto(produto1);
venda.adicionarProduto(produto2);

console.log(`Total da venda: R$ ${venda.calcularTotal().toFixed(2)}`);
```

Neste código, temos três classes principais: `Produto`, `ProdutoImportado` e `Venda`. A classe `Produto` implementa a interface `IProduto` e possui um método `calcularDesconto` que retorna 10% de desconto sobre o preço do produto.

A classe `ProdutoImportado` herda da classe `Produto` e possui um atributo adicional `taxaImportacao`, que representa a taxa de importação do produto. No método `calcularDesconto`, é subtraído o valor da taxa de importação do desconto calculado pela classe pai.

A classe `Venda` utiliza o padrão de projeto Singleton para garantir que apenas uma instância da classe seja criada. Ela possui um array de produtos e métodos para adicionar produtos e calcular o total da venda. O método `calcularTotal` percorre todos os produtos, subtraindo o desconto de cada um do seu preço e acumulando o resultado no total da venda.

No exemplo de uso, é criada uma instância da classe `Venda`, são criados dois produtos (um `Produto` e um `ProdutoImportado`) e eles são adicionados à venda. Em seguida, é chamado o método `calcularTotal` para obter o valor total da venda, que é exibido no console.