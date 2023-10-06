Claro! Aqui está um código complexo em JavaScript que utiliza uma estrutura de dados chamada árvore binária de busca para organizar e pesquisar elementos:

```javascript
// Definindo a estrutura do nó da árvore
class Node {
  constructor(value) {
    this.value = value;
    this.left = null;
    this.right = null;
  }
}

// Definindo a árvore binária de busca
class BinarySearchTree {
  constructor() {
    this.root = null;
  }

  // Método para inserir um novo nó na árvore
  insert(value) {
    const newNode = new Node(value);

    if (this.root === null) {
      this.root = newNode;
    } else {
      this.insertNode(this.root, newNode);
    }
  }

  insertNode(node, newNode) {
    if (newNode.value < node.value) {
      if (node.left === null) {
        node.left = newNode;
      } else {
        this.insertNode(node.left, newNode);
      }
    } else {
      if (node.right === null) {
        node.right = newNode;
      } else {
        this.insertNode(node.right, newNode);
      }
    }
  }

  // Método para buscar um valor na árvore
  search(value) {
    return this.searchNode(this.root, value);
  }

  searchNode(node, value) {
    if (node === null) {
      return false;
    }

    if (value === node.value) {
      return true;
    }

    if (value < node.value) {
      return this.searchNode(node.left, value);
    } else {
      return this.searchNode(node.right, value);
    }
  }

  // Método para percorrer a árvore em ordem
  inorderTraversal() {
    const result = [];
    this.inorderTraversalNode(this.root, result);
    return result;
  }

  inorderTraversalNode(node, result) {
    if (node !== null) {
      this.inorderTraversalNode(node.left, result);
      result.push(node.value);
      this.inorderTraversalNode(node.right, result);
    }
  }
}

// Exemplo de uso da árvore binária de busca
const tree = new BinarySearchTree();
tree.insert(50);
tree.insert(30);
tree.insert(70);
tree.insert(20);
tree.insert(40);
tree.insert(60);
tree.insert(80);

console.log(tree.search(60));  // true
console.log(tree.search(90));  // false

console.log(tree.inorderTraversal());  // [20, 30, 40, 50, 60, 70, 80]
```

Neste código, criamos duas classes: `Node` e `BinarySearchTree`. A classe `Node` representa um nó da árvore, com uma propriedade `value`, e referências para os nós à esquerda (`left`) e à direita (`right`). A classe `BinarySearchTree` representa a própria árvore, com uma propriedade `root` representando o nó raiz.

Em seguida, definimos o método `insert` para inserir novos nós na árvore. Ele verifica se a árvore está vazia e, caso esteja, define o novo nó como raiz. Caso contrário, chama o método `insertNode` para inserir o novo nó na posição correta da árvore.

O método `insertNode` é responsável por percorrer a árvore de forma recursiva, comparando o valor do novo nó com o valor do nó atual. Se o valor for menor, o método é chamado recursivamente para o nó à esquerda. Caso contrário, é chamado para o nó à direita. Essa recursão continua até encontrar uma posição vazia para inserir o novo nó.

Em seguida, definimos o método `search` para buscar um valor na árvore. Ele chama o método `searchNode` para percorrer a árvore de forma recursiva, comparando o valor buscado com o valor do nó atual. Se o valor for encontrado, retorna `true`. Caso contrário, retorna `false`. O método `searchNode` também faz a recursão para os nós à esquerda e à direita, dependendo do valor buscado.

Por fim, definimos o método `inorderTraversal` para percorrer a árvore em ordem. Ele chama o método `inorderTraversalNode` para percorrer a árvore de forma recursiva, primeiro percorrendo os nós à esquerda, em seguida, adicionando o valor do nó atual ao resultado e, por fim, percorrendo os nós à direita. O resultado é um array com os valores da árvore em ordem.

No exemplo de uso, criamos uma instância da árvore, inserimos alguns valores e realizamos buscas e percorremos a árvore em ordem.