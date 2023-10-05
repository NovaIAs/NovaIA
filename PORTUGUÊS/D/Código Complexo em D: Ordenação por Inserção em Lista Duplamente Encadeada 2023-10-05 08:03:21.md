Claro, vou criar um código complexo em D para você. Este código é um exemplo de um programa que implementa a ordenação por inserção em uma lista duplamente encadeada.

```d
import std.stdio;

struct Node {
    int value;
    Node* prev;
    Node* next;
}

void insert(Node** head, int newValue) {
    auto newNode = new Node;
    newNode.value = newValue;
    newNode.prev = null;
    newNode.next = null;

    if (*head is null) {
        *head = newNode;
        return;
    }

    auto current = *head;
    while (current.next !is null && current.next.value < newValue) {
        current = current.next;
    }

    if (current.prev is null) {
        newNode.next = *head;
        *head.prev = newNode;
        *head = newNode;
    } else {
        newNode.prev = current;
        newNode.next = current.next;
        if (current.next !is null) {
            current.next.prev = newNode;
        }
        current.next = newNode;
    }
}

void printList(Node* head) {
    auto current = head;
    while (current !is null) {
        writefln("%d", current.value);
        current = current.next;
    }
}

void main() {
    Node* head = null;

    // Inserção dos valores na lista
    insert(&head, 5);
    insert(&head, 2);
    insert(&head, 7);
    insert(&head, 3);
    insert(&head, 1);

    // Impressão da lista ordenada
    writefln("Lista ordenada:");
    printList(head);
}
```

Neste código, temos a definição de uma estrutura de dados chamada `Node`, que representa um nó em uma lista duplamente encadeada. Cada nó contém um valor inteiro, bem como referências para o nó anterior (`prev`) e o próximo nó (`next`).

A função `insert` é responsável por inserir um novo valor na lista, mantendo-a ordenada em ordem crescente. Ela recebe um ponteiro para o ponteiro da cabeça da lista (`head`) e o novo valor a ser inserido. Primeiro, cria-se um novo nó com o valor fornecido e as referências definidas como nulas.

Em seguida, verifica-se se a lista está vazia. Se estiver, o novo nó se torna a cabeça da lista e a função retorna.

Caso contrário, percorre-se a lista até encontrar a posição correta para inserir o novo valor. Isso é feito comparando o valor do nó atual com o novo valor. Se o valor do próximo nó for menor, continua-se percorrendo a lista. Quando a posição correta é encontrada, o novo nó é inserido nessa posição.

Se a posição correta for a primeira da lista (ou seja, o novo valor é menor do que o valor atual da cabeça da lista), os ponteiros são ajustados para que o novo nó se torne a nova cabeça.

A função `printList` é responsável por imprimir os valores da lista encadeada. Ela recebe um ponteiro para a cabeça da lista (`head`) e percorre a lista, imprimindo cada valor.

No final, no `main`, é criado um ponteiro para a cabeça da lista, inicializado como nulo. São realizadas cinco inserções na lista com valores diferentes. Em seguida, a lista ordenada é impressa na tela.

Espero que este código complexo em D atenda às suas expectativas!