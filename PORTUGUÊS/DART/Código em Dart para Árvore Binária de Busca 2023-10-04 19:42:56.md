Claro! Abaixo está um código complexo em Dart que cria uma estrutura de dados chamada árvore binária de busca:

```dart
class Node {
  int value;
  Node left;
  Node right;

  Node(this.value);
}

class BinarySearchTree {
  Node root;

  BinarySearchTree();

  void insert(int value) {
    Node newNode = Node(value);

    if (root == null) {
      root = newNode;
    } else {
      _insertNode(root, newNode);
    }
  }

  void _insertNode(Node node, Node newNode) {
    if (newNode.value < node.value) {
      if (node.left == null) {
        node.left = newNode;
      } else {
        _insertNode(node.left, newNode);
      }
    } else {
      if (node.right == null) {
        node.right = newNode;
      } else {
        _insertNode(node.right, newNode);
      }
    }
  }

  bool search(int value) {
    return _searchNode(root, value);
  }

  bool _searchNode(Node node, int value) {
    if (node == null) {
      return false;
    } else if (node.value == value) {
      return true;
    } else if (value < node.value) {
      return _searchNode(node.left, value);
    } else {
      return _searchNode(node.right, value);
    }
  }

  void remove(int value) {
    root = _removeNode(root, value);
  }

  Node _removeNode(Node node, int value) {
    if (node == null) {
      return null;
    } else if (value < node.value) {
      node.left = _removeNode(node.left, value);
      return node;
    } else if (value > node.value) {
      node.right = _removeNode(node.right, value);
      return node;
    } else {
      if (node.left == null && node.right == null) {
        return null;
      } else if (node.left == null) {
        return node.right;
      } else if (node.right == null) {
        return node.left;
      } else {
        Node minRight = _findMinNode(node.right);
        node.value = minRight.value;
        node.right = _removeNode(node.right, minRight.value);
        return node;
      }
    }
  }

  Node _findMinNode(Node node) {
    if (node.left == null) {
      return node;
    } else {
      return _findMinNode(node.left);
    }
  }

  void printTree() {
    _printNode(root);
  }

  void _printNode(Node node) {
    if (node != null) {
      _printNode(node.left);
      print(node.value);
      _printNode(node.right);
    }
  }
}

void main() {
  BinarySearchTree bst = BinarySearchTree();
  bst.insert(50);
  bst.insert(30);
  bst.insert(70);
  bst.insert(20);
  bst.insert(40);
  bst.insert(60);
  bst.insert(80);

  bst.printTree(); // Imprimirá os valores da árvore em ordem crescente: 20, 30, 40, 50, 60, 70, 80

  print(bst.search(40)); // Imprimirá true

  bst.remove(30);
  bst.printTree(); // Imprimirá os valores da árvore em ordem crescente após remover o valor 30: 20, 40, 50, 60, 70, 80
}
```

Nesse código, criamos a classe `Node`, que representa um nó da árvore binária de busca, contendo um valor inteiro e referências para os nós da esquerda e direita. A classe `BinarySearchTree` é responsável por implementar as operações de inserção, busca e remoção na árvore.

Na função `insert`, verificamos se a árvore está vazia. Se estiver, o novo nó é inserido como raiz. Caso contrário, chamamos a função `_insertNode` para percorrer a árvore e encontrar a posição correta para inserir o novo nó.

A função `_insertNode` é uma função auxiliar recursiva que percorre a árvore, comparando o valor do novo nó com o valor do nó atual. Caso o valor seja menor, o novo nó é inserido na subárvore esquerda. Caso contrário, é inserido na subárvore direita.

A função `search` também utiliza uma função auxiliar recursiva `_searchNode` para percorrer a árvore em busca de um determinado valor. Se o valor for encontrado, retorna true. Caso contrário, retorna false.

A função `remove` é responsável por remover um valor da árvore. Ela utiliza a função `_removeNode` para encontrar o nó com o valor a ser removido e realiza a remoção de acordo com os casos possíveis: o nó não possui filhos, o nó possui apenas um filho ou o nó possui dois filhos.

A função `_findMinNode` é utilizada para encontrar o nó com o valor mínimo em uma subárvore, que é necessário para a remoção de um nó com dois filhos.

Por fim, temos a função `printTree`, que utiliza a função `_printNode` para imprimir os valores da árvore em ordem crescente.

No exemplo principal, criamos uma instância da classe `BinarySearchTree`, inserimos alguns valores na árvore, imprimimos os valores em ordem crescente, realizamos uma busca pelo valor 40, removemos o valor 30 e imprimimos novamente os valores em ordem crescente.