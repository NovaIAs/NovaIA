Claro! Vou gerar um código complexo em D que envolve a implementação de um algoritmo de pesquisa binária utilizando uma árvore binária de pesquisa. Aqui está o código:

```d
import std.stdio;

struct Node {
    int value;
    Node* left;
    Node* right;
}

Node* insert(Node* root, int value) {
    if (root is null) {
        Node* newNode = new Node;
        newNode.value = value;
        newNode.left = null;
        newNode.right = null;
        return newNode;
    }

    if (value < root.value) {
        root.left = insert(root.left, value);
    } else if (value > root.value) {
        root.right = insert(root.right, value);
    }
    
    return root;
}

bool search(Node* root, int value) {
    if (root is null) {
        return false;
    }

    if (value == root.value) {
        return true;
    } else if (value < root.value) {
        return search(root.left, value);
    } else {
        return search(root.right, value);
    }
}

void printInOrder(Node* root) {
    if (root is null) {
        return;
    }

    printInOrder(root.left);
    writeln(root.value);
    printInOrder(root.right);
}

void main() {
    Node* root = null;

    root = insert(root, 5);
    root = insert(root, 3);
    root = insert(root, 7);
    root = insert(root, 1);
    root = insert(root, 4);
    root = insert(root, 6);
    root = insert(root, 9);

    writeln("Árvore em ordem:");
    printInOrder(root);

    int value = 4;
    bool found = search(root, value);
    
    if (found) {
        writeln("O valor ", value, " foi encontrado na árvore.");
    } else {
        writeln("O valor ", value, " não foi encontrado na árvore.");
    }
}
```

Este código em D implementa um algoritmo de pesquisa binária usando uma árvore binária de pesquisa. A estrutura `Node` representa um nó da árvore, contendo um valor inteiro, um ponteiro para o nó à esquerda e um ponteiro para o nó à direita.

A função `insert` é responsável por inserir um valor na árvore de forma recursiva, seguindo as regras da pesquisa binária. Se o nó atual for nulo, um novo nó é criado com o valor informado. Caso contrário, o valor é comparado com o valor do nó atual e a função é chamada recursivamente para o nó à esquerda ou à direita, dependendo do valor.

A função `search` realiza a busca por um valor na árvore de forma recursiva. Se o nó atual for nulo, significa que o valor não foi encontrado e retorna-se `false`. Se o valor for igual ao valor do nó atual, retorna-se `true`. Caso contrário, a função é chamada recursivamente para o nó à esquerda ou à direita, dependendo do valor.

A função `printInOrder` imprime os valores da árvore em ordem crescente, seguindo a ordem esquerda-raiz-direita. Para isso, a função é chamada recursivamente para o nó à esquerda, imprime o valor do nó atual e, em seguida, é chamada recursivamente para o nó à direita.

Na função `main`, é criado um nó raiz nulo e são inseridos alguns valores na árvore. Em seguida, a função `printInOrder` é chamada para imprimir os valores em ordem. Por fim, é feita uma busca pelo valor 4 na árvore e é exibida uma mensagem indicando se o valor foi encontrado ou não.

Espero que este código atenda às suas expectativas!