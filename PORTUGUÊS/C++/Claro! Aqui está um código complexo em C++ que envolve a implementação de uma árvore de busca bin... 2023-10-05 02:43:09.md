Claro! Aqui está um código complexo em C++ que envolve a implementação de uma árvore de busca binária balanceada, conhecida como Árvore AVL. A árvore AVL é uma estrutura de dados eficiente que mantém o balanceamento automático dos seus nós, garantindo um tempo de busca, inserção e remoção de elementos em O(log n), onde n é o número de elementos na árvore.

```cpp
#include <iostream>

// Definição da estrutura do nó da Árvore AVL
struct Node {
    int key;
    int height;
    Node* left;
    Node* right;
};

// Função para criar um novo nó
Node* criarNo(int key) {
    Node* node = new Node();
    node->key = key;
    node->height = 1;
    node->left = nullptr;
    node->right = nullptr;
    return node;
}

// Função para obter a altura de um nó
int getHeight(Node* node) {
    if (node == nullptr) {
        return 0;
    }
    return node->height;
}

// Função para atualizar a altura de um nó
void updateHeight(Node* node) {
    int leftHeight = getHeight(node->left);
    int rightHeight = getHeight(node->right);
    node->height = std::max(leftHeight, rightHeight) + 1;
}

// Função para calcular o fator de balanceamento de um nó
int getBalanceFactor(Node* node) {
    if (node == nullptr) {
        return 0;
    }
    return getHeight(node->left) - getHeight(node->right);
}

// Função para realizar uma rotação simples à direita
Node* rotateRight(Node* node) {
    Node* newRoot = node->left;
    node->left = newRoot->right;
    newRoot->right = node;
    updateHeight(node);
    updateHeight(newRoot);
    return newRoot;
}

// Função para realizar uma rotação simples à esquerda
Node* rotateLeft(Node* node) {
    Node* newRoot = node->right;
    node->right = newRoot->left;
    newRoot->left = node;
    updateHeight(node);
    updateHeight(newRoot);
    return newRoot;
}

// Função para balancear um nó
Node* balance(Node* node) {
    updateHeight(node);
    int balanceFactor = getBalanceFactor(node);
    if (balanceFactor > 1) {
        if (getBalanceFactor(node->left) < 0) {
            node->left = rotateLeft(node->left);
        }
        return rotateRight(node);
    } else if (balanceFactor < -1) {
        if (getBalanceFactor(node->right) > 0) {
            node->right = rotateRight(node->right);
        }
        return rotateLeft(node);
    }
    return node;
}

// Função para inserir um elemento na árvore AVL
Node* insert(Node* node, int key) {
    if (node == nullptr) {
        return criarNo(key);
    }
    if (key < node->key) {
        node->left = insert(node->left, key);
    } else if (key > node->key) {
        node->right = insert(node->right, key);
    } else {
        return node; // Chaves iguais não são permitidas
    }
    return balance(node);
}

// Função para imprimir a árvore AVL (percurso em ordem)
void printTree(Node* node) {
    if (node != nullptr) {
        printTree(node->left);
        std::cout << node->key << " ";
        printTree(node->right);
    }
}

int main() {
    Node* root = nullptr;

    // Inserção de elementos na árvore AVL
    root = insert(root, 10);
    root = insert(root, 20);
    root = insert(root, 30);
    root = insert(root, 40);
    root = insert(root, 50);
    root = insert(root, 25);

    // Impressão da árvore AVL
    std::cout << "Árvore AVL: ";
    printTree(root);

    return 0;
}
```

Este código implementa uma árvore AVL e demonstra a inserção de elementos na árvore. Primeiro, são definidos os structs Node, que representa um nó da árvore AVL, e em seguida, são implementadas funções para criar um novo nó, obter a altura de um nó, atualizar a altura de um nó, calcular o fator de balanceamento de um nó e realizar rotações simples à direita e à esquerda.

A função de balanceamento é responsável por verificar e ajustar o balanceamento da árvore AVL após a inserção de um elemento. Caso o fator de balanceamento de um nó esteja fora do intervalo [-1, 1], são realizadas rotações para equilibrar a árvore.

A função de inserção insere um elemento na árvore AVL, respeitando as propriedades da árvore. Caso a chave a ser inserida já exista na árvore, ela não será inserida novamente.

Por fim, a função `printTree` é utilizada para imprimir os elementos da árvore AVL em ordem. No exemplo fornecido, são inseridos os elementos 10, 20, 30, 40, 50 e 25 na árvore, e a função `printTree` é utilizada para exibir os elementos em ordem.

Espero que este código complexo tenha atendido às suas expectativas! Se você tiver alguma dúvida adicional ou precisar de mais exemplos, por favor, me avise.