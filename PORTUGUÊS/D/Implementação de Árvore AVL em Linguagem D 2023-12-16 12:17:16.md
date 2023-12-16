Claro! Aqui está um código complexo em linguagem D, que implementa uma árvore AVL (Adelson-Velsky e Landis), uma estrutura de dados balanceada que permite inserções, remoções e buscas eficientes:

```d
import std.stdio;

struct Node {
    int value;
    Node* left;
    Node* right;
    int height;
}

Node* createNode(int value) {
    auto node = new Node;
    node.value = value;
    node.left = null;
    node.right = null;
    node.height = 1;
    return node;
}

int getHeight(Node* node) {
    if (node == null) {
        return 0;
    }
    return node.height;
}

int getBalanceFactor(Node* node) {
    if (node == null) {
        return 0;
    }
    return getHeight(node.left) - getHeight(node.right);
}

Node* rotateRight(Node* y) {
    auto x = y.left;
    auto temp = x.right;
    x.right = y;
    y.left = temp;
    y.height = max(getHeight(y.left), getHeight(y.right)) + 1;
    x.height = max(getHeight(x.left), getHeight(x.right)) + 1;
    return x;
}

Node* rotateLeft(Node* x) {
    auto y = x.right;
    auto temp = y.left;
    y.left = x;
    x.right = temp;
    x.height = max(getHeight(x.left), getHeight(x.right)) + 1;
    y.height = max(getHeight(y.left), getHeight(y.right)) + 1;
    return y;
}

Node* insert(Node* node, int value) {
    if (node == null) {
        return createNode(value);
    }
    
    if (value < node.value) {
        node.left = insert(node.left, value);
    } else if (value > node.value) {
        node.right = insert(node.right, value);
    } else {
        return node; // Valor duplicado, não é permitido
    }
    
    node.height = max(getHeight(node.left), getHeight(node.right)) + 1;
    
    int balanceFactor = getBalanceFactor(node);
    
    if (balanceFactor > 1 && value < node.left.value) {
        return rotateRight(node);
    }
    
    if (balanceFactor < -1 && value > node.right.value) {
        return rotateLeft(node);
    }
    
    if (balanceFactor > 1 && value > node.left.value) {
        node.left = rotateLeft(node.left);
        return rotateRight(node);
    }
    
    if (balanceFactor < -1 && value < node.right.value) {
        node.right = rotateRight(node.right);
        return rotateLeft(node);
    }
    
    return node;
}

void printTree(Node* node, int indent = 0) {
    if (node != null) {
        printTree(node.right, indent + 4);
        writeln(" ".repeat(indent), node.value);
        printTree(node.left, indent + 4);
    }
}

void main() {
    Node* root = null;
    
    root = insert(root, 10);
    root = insert(root, 20);
    root = insert(root, 30);
    root = insert(root, 40);
    root = insert(root, 50);
    root = insert(root, 25);
    
    writeln("Árvore AVL balanceada:");
    printTree(root);
}
```

Este código em D implementa uma árvore AVL com inserção e impressão da árvore resultante. A função `insert` insere um valor na árvore AVL balanceada, mantendo-a balanceada automaticamente. As funções `rotateRight` e `rotateLeft` são usadas para executar as rotações necessárias para manter o balanceamento. A função `printTree` imprime a árvore de forma visualmente agradável, com os nós da direita sendo impressos primeiro, seguidos pelo nó atual e, por fim, os nós da esquerda.