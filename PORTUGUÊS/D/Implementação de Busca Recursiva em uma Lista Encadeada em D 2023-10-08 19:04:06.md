Certamente! Aqui está um código complexo em linguagem D que demonstra um exemplo de um algoritmo de busca em uma lista encadeada de forma recursiva:

```D
import std.stdio;

struct Node {
    int data;
    Node* next;
}

Node* insert(Node* head, int data) {
    Node* newNode = new Node;
    newNode.data = data;
    newNode.next = null;

    if (head is null) {
        head = newNode;
    } else {
        Node* temp = head;
        while (temp.next !is null) {
            temp = temp.next;
        }
        temp.next = newNode;
    }
    return head;
}

Node* search(Node* head, int key) {
    if (head is null || head.data == key) {
        return head;
    }
    return search(head.next, key);
}

void display(Node* head) {
    Node* temp = head;
    while (temp !is null) {
        writef("%d ", temp.data);
        temp = temp.next;
    }
}

void main() {
    Node* head = null;

    head = insert(head, 10);
    head = insert(head, 20);
    head = insert(head, 30);
    head = insert(head, 40);
    head = insert(head, 50);

    writef("Lista: ");
    display(head);

    int key = 30;
    Node* result = search(head, key);

    if (result !is null) {
        writef("\n%d encontrado na lista.", key);
    } else {
        writef("\n%d não encontrado na lista.", key);
    }
}
```

Este código demonstra uma implementação de uma lista encadeada em D. A função `insert` é usada para inserir elementos na lista encadeada, enquanto a função `search` é usada para procurar um elemento específico na lista. A função `display` é usada para exibir todos os elementos da lista.

No `main`, são inseridos alguns valores na lista e é feita uma busca pelo valor 30. Se o valor for encontrado, a mensagem "30 encontrado na lista" será exibida, caso contrário, a mensagem "30 não encontrado na lista" será exibida.

Espero que este código complexo em D seja útil para você!