Claro! Aqui está um exemplo de código em Objective-C que cria uma estrutura de dados de lista encadeada, contendo funções para adicionar elementos, remover elementos e imprimir a lista:

```objective-c
// Definindo a estrutura de um nó da lista
typedef struct Node {
    int data;
    struct Node *next;
} Node;

// Definindo a estrutura da lista encadeada
typedef struct List {
    Node *head;
} List;

// Função para criar um novo nó com o dado fornecido
Node* createNode(int data) {
    Node *newNode = (Node*) malloc(sizeof(Node));
    newNode->data = data;
    newNode->next = NULL;
    return newNode;
}

// Função para adicionar um elemento no início da lista
void addElement(List *list, int data) {
    Node *newNode = createNode(data);
    if (list->head == NULL) {
        list->head = newNode;
    } else {
        newNode->next = list->head;
        list->head = newNode;
    }
}

// Função para remover um elemento da lista
void removeElement(List *list, int data) {
    if (list->head == NULL) {
        printf("A lista está vazia!\n");
        return;
    }
    
    Node *current = list->head;
    Node *prev = NULL;
    
    while (current != NULL && current->data != data) {
        prev = current;
        current = current->next;
    }
    
    if (current == NULL) {
        printf("Elemento não encontrado!\n");
        return;
    }
    
    if (prev == NULL) {
        list->head = current->next;
    } else {
        prev->next = current->next;
    }
    
    free(current);
}

// Função para imprimir os elementos da lista
void printList(List *list) {
    if (list->head == NULL) {
        printf("A lista está vazia!\n");
        return;
    }
    
    Node *current = list->head;
    
    while (current != NULL) {
        printf("%d ", current->data);
        current = current->next;
    }
    
    printf("\n");
}

// Função para liberar a memória ocupada pela lista
void freeList(List *list) {
    Node *current = list->head;
    
    while (current != NULL) {
        Node *temp = current->next;
        free(current);
        current = temp;
    }
    
    list->head = NULL;
}

int main() {
    List *list = (List*) malloc(sizeof(List));
    list->head = NULL;
    
    addElement(list, 5);
    addElement(list, 10);
    addElement(list, 15);
    
    printf("Lista inicial: ");
    printList(list);
    
    removeElement(list, 10);
    
    printf("Lista após remoção: ");
    printList(list);
    
    freeList(list);
    free(list);
    
    return 0;
}
```

Este código cria uma lista encadeada em Objective-C e oferece funções para adicionar, remover e imprimir os elementos da lista. A função `addElement` adiciona um novo elemento no início da lista, a função `removeElement` remove um elemento específico da lista, a função `printList` imprime os elementos da lista e a função `freeList` libera a memória ocupada pela lista. No `main`, criamos uma lista, adicionamos alguns elementos, removemos um elemento e, finalmente, liberamos a memória antes de encerrar o programa.