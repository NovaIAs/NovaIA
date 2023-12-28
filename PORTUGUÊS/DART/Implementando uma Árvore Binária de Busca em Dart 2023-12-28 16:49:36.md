Claro! Aqui está um código em Dart que implementa uma árvore binária de busca:

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
    }

    if (node.value == value) {
      return true;
    }

    if (value < node.value) {
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
    }

    if (value < node.value) {
      node.left = _removeNode(node.left, value);
    } else if (value > node.value) {
      node.right = _removeNode(node.right, value);
    } else {
      if (node.left == null && node.right == null) {
        node = null;
      } else if (node.left == null) {
        node = node.right;
      } else if (node.right == null) {
        node = node.left;
      } else {
        Node minNode = _findMinNode(node.right);
        node.value = minNode.value;
        node.right = _removeNode(node.right, minNode.value);
      }
    }

    return node;
  }

  Node _findMinNode(Node node) {
    if (node.left != null) {
      return _findMinNode(node.left);
    }
    return node;
  }
}

void main() {
  BinarySearchTree bst = BinarySearchTree();
  
  bst.insert(8);
  bst.insert(3);
  bst.insert(10);
  bst.insert(1);
  bst.insert(6);
  bst.insert(14);
  bst.insert(4);
  bst.insert(7);
  bst.insert(13);
  
  print(bst.search(6)); // Output: true
  print(bst.search(12)); // Output: false
  
  bst.remove(6);
  print(bst.search(6)); // Output: false
}
```

Neste código, temos a definição das classes `Node` e `BinarySearchTree`. A classe `Node` representa um nó da árvore, com um valor inteiro e referências para os nós esquerdo e direito. A classe `BinarySearchTree` representa a árvore binária de busca, com um atributo `root` para o nó raiz.

O código implementa três principais operações: `insert`, `search` e `remove`. A operação `insert` insere um novo valor na árvore, respeitando a estrutura da árvore binária de busca. A operação `search` busca um valor específico na árvore e retorna `true` se o valor for encontrado e `false` caso contrário. A operação `remove` remove um valor da árvore, mantendo a estrutura da árvore binária de busca.

No final do código, há um exemplo de uso da árvore binária de busca, onde alguns valores são inseridos na árvore, é feita uma busca por dois valores diferentes e um valor é removido.