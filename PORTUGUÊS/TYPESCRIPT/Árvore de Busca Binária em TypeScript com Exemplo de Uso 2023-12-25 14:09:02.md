Claro! Abaixo está um exemplo de código em TypeScript que implementa uma árvore de busca binária e realiza a busca por um valor específico:

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

class ArvoreBinaria {
    raiz: No | null;

    constructor() {
        this.raiz = null;
    }

    // Método para adicionar um novo nó na árvore
    adicionar(valor: number): void {
        const novoNo = new No(valor);

        if (this.raiz === null) {
            this.raiz = novoNo;
        } else {
            this.adicionarNo(this.raiz, novoNo);
        }
    }

    private adicionarNo(noAtual: No, novoNo: No): void {
        if (novoNo.valor < noAtual.valor) {
            if (noAtual.esquerda === null) {
                noAtual.esquerda = novoNo;
            } else {
                this.adicionarNo(noAtual.esquerda, novoNo);
            }
        } else {
            if (noAtual.direita === null) {
                noAtual.direita = novoNo;
            } else {
                this.adicionarNo(noAtual.direita, novoNo);
            }
        }
    }

    // Método para buscar um valor na árvore
    buscar(valor: number): boolean {
        return this.buscarNo(this.raiz, valor);
    }

    private buscarNo(noAtual: No | null, valor: number): boolean {
        if (noAtual === null) {
            return false;
        }

        if (valor === noAtual.valor) {
            return true;
        }

        if (valor < noAtual.valor) {
            return this.buscarNo(noAtual.esquerda, valor);
        } else {
            return this.buscarNo(noAtual.direita, valor);
        }
    }
}

// Exemplo de uso da árvore de busca binária
const arvore = new ArvoreBinaria();
arvore.adicionar(10);
arvore.adicionar(5);
arvore.adicionar(15);
arvore.adicionar(2);
arvore.adicionar(7);

console.log(arvore.buscar(5)); // true
console.log(arvore.buscar(12)); // false
console.log(arvore.buscar(2)); // true
```

Neste exemplo, temos a classe `No` que representa um nó da árvore, com um valor e referências para os nós esquerdo e direito. A classe `ArvoreBinaria` implementa a árvore de busca binária, com os métodos `adicionar` para adicionar um novo valor à árvore e `buscar` para buscar um valor específico na árvore.

O método `adicionar` percorre a árvore de forma recursiva, comparando o valor a ser adicionado com o valor do nó atual e seguindo para a esquerda ou direita, conforme necessário. O método `buscar` também percorre a árvore de forma recursiva, comparando o valor a ser buscado com o valor do nó atual e seguindo para a esquerda ou direita, até encontrar o valor desejado ou chegar a um nó nulo.

No exemplo de uso, criamos uma árvore de busca binária, adicionamos alguns valores e em seguida fazemos algumas buscas na árvore. Os resultados são exibidos no console.