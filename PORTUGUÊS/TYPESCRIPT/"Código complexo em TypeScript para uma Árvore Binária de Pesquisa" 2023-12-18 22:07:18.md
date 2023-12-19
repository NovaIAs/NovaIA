Claro! Aqui está um código complexo em TypeScript que utiliza classes, herança e interfaces para criar uma estrutura de dados de uma árvore binária de pesquisa:

```typescript
interface Node<T> {
  value: T;
  left?: Node<T>;
  right?: Node<T>;
}

class BinaryTree<T> {
  private root?: Node<T>;

  public insert(value: T): void {
    const newNode: Node<T> = { value };

    if (!this.root) {
      this.root = newNode;
    } else {
      this.insertNode(this.root, newNode);
    }
  }

  private insertNode(node: Node<T>, newNode: Node<T>): void {
    if (newNode.value < node.value) {
      if (!node.left) {
        node.left = newNode;
      } else {
        this.insertNode(node.left, newNode);
      }
    } else {
      if (!node.right) {
        node.right = newNode;
      } else {
        this.insertNode(node.right, newNode);
      }
    }
  }

  public search(value: T): boolean {
    return this.searchNode(this.root, value);
  }

  private searchNode(node: Node<T> | undefined, value: T): boolean {
    if (!node) {
      return false;
    }

    if (value < node.value) {
      return this.searchNode(node.left, value);
    } else if (value > node.value) {
      return this.searchNode(node.right, value);
    } else {
      return true;
    }
  }
}

// Exemplo de uso
const tree = new BinaryTree<number>();
tree.insert(8);
tree.insert(3);
tree.insert(10);
tree.insert(1);
tree.insert(6);
tree.insert(14);
tree.insert(4);
tree.insert(7);
tree.insert(13);

console.log(tree.search(6)); // true
console.log(tree.search(11)); // false
```

Neste código, a classe `BinaryTree` representa uma árvore binária de pesquisa. A interface `Node` define a estrutura de cada nó da árvore, contendo um valor e referências para os nós da esquerda e da direita. A classe possui um método `insert` que insere um novo valor na árvore de forma ordenada, respeitando a propriedade da árvore binária de pesquisa. O método `search` verifica se um valor está presente na árvore.

No exemplo de uso, criamos uma instância da classe `BinaryTree`, inserimos alguns valores e depois realizamos buscas por valores específicos.