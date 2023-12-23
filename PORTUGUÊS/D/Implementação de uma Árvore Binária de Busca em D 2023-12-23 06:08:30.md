Claro! Abaixo está um exemplo de um código complexo na linguagem D. Este código implementa uma árvore binária de busca e inclui operações de inserção, busca e remoção de elementos. Vou explicar cada parte do código para facilitar o entendimento.

```d
import std.stdio;

struct Node {
    int value;
    Node left, right;
}

void insert(ref Node root, int value) {
    if (root is null) {
        root = Node(value);
    } else if (value < root.value) {
        insert(root.left, value);
    } else if (value > root.value) {
        insert(root.right, value);
    } else {
        writeln("O valor ", value, " já existe na árvore.");
    }
}

bool search(Node root, int value) {
    if (root is null) {
        return false;
    } else if (value < root.value) {
        return search(root.left, value);
    } else if (value > root.value) {
        return search(root.right, value);
    } else {
        return true;
    }
}

void remove(ref Node root, int value) {
    if (root is null) {
        writeln("O valor ", value, " não existe na árvore.");
    } else if (value < root.value) {
        remove(root.left, value);
    } else if (value > root.value) {
        remove(root.right, value);
    } else {
        if (root.left is null) {
            root = root.right;
        } else if (root.right is null) {
            root = root.left;
        } else {
            Node tmp = root.right;
            while (tmp.left !is null) {
                tmp = tmp.left;
            }
            root.value = tmp.value;
            remove(root.right, tmp.value);
        }
    }
}

void printInOrder(Node root) {
    if (root !is null) {
        printInOrder(root.left);
        write(root.value, " ");
        printInOrder(root.right);
    }
}

void main() {
    Node root = null;

    insert(root, 10);
    insert(root, 5);
    insert(root, 15);
    insert(root, 3);
    insert(root, 7);
    insert(root, 12);
    insert(root, 18);
    insert(root, 1);
    insert(root, 4);
    insert(root, 6);
    insert(root, 9);
    insert(root, 13);
    insert(root, 17);
    insert(root, 20);

    writeln("Árvore em ordem crescente:");
    printInOrder(root);
    writeln();

    writeln("Removendo o valor 5:");
    remove(root, 5);

    writeln("Árvore em ordem crescente após remoção:");
    printInOrder(root);
    writeln();

    writeln("Buscando o valor 15:");
    if (search(root, 15)) {
        writeln("O valor 15 foi encontrado na árvore.");
    } else {
        writeln("O valor 15 não foi encontrado na árvore.");
    }
}
```

Explicação do código:
- A estrutura `Node` representa um nó da árvore binária de busca, contendo um valor inteiro e duas referências para os nós filhos esquerdo e direito.
- A função `insert` insere um valor na árvore binária de busca, percorrendo a árvore de forma recursiva e comparando o valor a ser inserido com o valor do nó atual. Se o valor já existir na árvore, uma mensagem é exibida.
- A função `search` busca um valor na árvore binária de busca, percorrendo a árvore de forma recursiva e comparando o valor a ser buscado com o valor do nó atual. Se o valor for encontrado, a função retorna `true`; caso contrário, retorna `false`.
- A função `remove` remove um valor da árvore binária de busca, percorrendo a árvore de forma recursiva e comparando o valor a ser removido com o valor do nó atual. Existem três casos possíveis: se o nó a ser removido não tiver filhos, ele é removido diretamente; se o nó tiver apenas um filho, ele é substituído pelo filho; se o nó tiver dois filhos, é necessária uma substituição complexa.
- A função `printInOrder` imprime os valores da árvore em ordem crescente, percorrendo a árvore de forma recursiva e imprimindo os valores do nó atual antes de percorrer os filhos.
- No `main`, é criada uma árvore binária de busca e são realizadas algumas operações de inserção, remoção e busca. Em seguida, a árvore é impressa em ordem crescente e alguns valores são removidos e buscados novamente.

Espero que este código complexo em D atenda às suas expectativas! Se tiver mais alguma dúvida, estou à disposição.