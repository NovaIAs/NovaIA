Claro! Aqui está um exemplo de código complexo em TypeScript que envolve a criação de uma árvore binária de busca:

```typescript
class No {
  valor: number;
  esquerda: No | null;
  direita: No | null;

  constructor(valor: number) {
    this.valor = valor;
    this.esquerda = null;
    this.direita = null;
  }
}

class ArvoreBinariaBusca {
  raiz: No | null;

  constructor() {
    this.raiz = null;
  }

  adicionar(valor: number) {
    const novoNo = new No(valor);

    if (this.raiz === null) {
      this.raiz = novoNo;
    } else {
      this.adicionarNo(this.raiz, novoNo);
    }
  }

  private adicionarNo(no: No, novoNo: No) {
    if (novoNo.valor < no.valor) {
      if (no.esquerda === null) {
        no.esquerda = novoNo;
      } else {
        this.adicionarNo(no.esquerda, novoNo);
      }
    } else {
      if (no.direita === null) {
        no.direita = novoNo;
      } else {
        this.adicionarNo(no.direita, novoNo);
      }
    }
  }

  procurar(valor: number): boolean {
    return this.procurarNo(this.raiz, valor);
  }

  private procurarNo(no: No | null, valor: number): boolean {
    if (no === null) {
      return false;
    }

    if (valor === no.valor) {
      return true;
    } else if (valor < no.valor) {
      return this.procurarNo(no.esquerda, valor);
    } else {
      return this.procurarNo(no.direita, valor);
    }
  }

  imprimirEmOrdem() {
    this.imprimirEmOrdemNo(this.raiz);
  }

  private imprimirEmOrdemNo(no: No | null) {
    if (no !== null) {
      this.imprimirEmOrdemNo(no.esquerda);
      console.log(no.valor);
      this.imprimirEmOrdemNo(no.direita);
    }
  }
}

// Exemplo de uso da árvore binária de busca
const arvore = new ArvoreBinariaBusca();
arvore.adicionar(10);
arvore.adicionar(5);
arvore.adicionar(15);
arvore.adicionar(3);
arvore.adicionar(7);

console.log(arvore.procurar(7)); // true
console.log(arvore.procurar(12)); // false

arvore.imprimirEmOrdem();
```

Este código implementa uma árvore binária de busca em TypeScript. A classe `No` representa um nó da árvore, contendo um valor, uma referência para o nó à esquerda e uma referência para o nó à direita. A classe `ArvoreBinariaBusca` implementa as operações de adicionar um valor à árvore, procurar um valor na árvore e imprimir a árvore em ordem. 

No exemplo de uso no final do código, a árvore é criada e alguns valores são adicionados. Em seguida, é feita uma busca pelo valor 7, que retorna `true`, e pelo valor 12, que retorna `false`. Por fim, a árvore é impressa em ordem, resultando em:

```
3
5
7
10
15
```

Espero que esse código complexo atenda às suas expectativas!