Claro! Aqui está um exemplo de um código complexo em Objective-C que cria uma lista ligada duplamente encadeada:

```objective-c
#import <Foundation/Foundation.h>

@interface Node : NSObject

@property (nonatomic, strong) id data;
@property (nonatomic, strong) Node *previous;
@property (nonatomic, strong) Node *next;

@end

@implementation Node

@end

@interface DoublyLinkedList : NSObject

@property (nonatomic, strong) Node *head;
@property (nonatomic, strong) Node *tail;

- (void)insertAtHead:(id)data;
- (void)insertAtTail:(id)data;
- (void)removeAtHead;
- (void)removeAtTail;
- (void)printList;

@end

@implementation DoublyLinkedList

- (void)insertAtHead:(id)data {
    Node *newNode = [[Node alloc] init];
    newNode.data = data;
    
    if (self.head == nil) {
        self.head = newNode;
        self.tail = newNode;
    } else {
        newNode.next = self.head;
        self.head.previous = newNode;
        self.head = newNode;
    }
}

- (void)insertAtTail:(id)data {
    Node *newNode = [[Node alloc] init];
    newNode.data = data;
    
    if (self.tail == nil) {
        self.head = newNode;
        self.tail = newNode;
    } else {
        newNode.previous = self.tail;
        self.tail.next = newNode;
        self.tail = newNode;
    }
}

- (void)removeAtHead {
    if (self.head == nil) {
        NSLog(@"A lista está vazia.");
        return;
    }
    
    if (self.head == self.tail) {
        self.head = nil;
        self.tail = nil;
    } else {
        self.head = self.head.next;
        self.head.previous = nil;
    }
}

- (void)removeAtTail {
    if (self.tail == nil) {
        NSLog(@"A lista está vazia.");
        return;
    }
    
    if (self.head == self.tail) {
        self.head = nil;
        self.tail = nil;
    } else {
        self.tail = self.tail.previous;
        self.tail.next = nil;
    }
}

- (void)printList {
    Node *current = self.head;
    
    while (current != nil) {
        NSLog(@"%@", current.data);
        current = current.next;
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        DoublyLinkedList *list = [[DoublyLinkedList alloc] init];
        
        [list insertAtHead:@"Node 1"];
        [list insertAtHead:@"Node 2"];
        [list insertAtTail:@"Node 3"];
        [list removeAtTail];
        [list printList];
    }
    return 0;
}
```

Neste exemplo, temos duas classes: `Node` (nó) e `DoublyLinkedList` (lista ligada duplamente encadeada). A classe `Node` representa um nó da lista, contendo uma referência para o dado armazenado, o nó anterior e o próximo nó. A classe `DoublyLinkedList` representa a lista em si, contendo uma referência para a cabeça (primeiro nó) e para a cauda (último nó) da lista.

A lista ligada duplamente encadeada permite a inserção e remoção eficiente de elementos tanto no início quanto no fim da lista. O código mostra como inserir elementos no início e no fim da lista (`insertAtHead` e `insertAtTail`), como remover elementos do início e do fim (`removeAtHead` e `removeAtTail`) e como imprimir todos os elementos da lista (`printList`).

No exemplo `main`, é criada uma instância da lista, inseridos dois nós no início, um nó no fim, e então o nó do fim é removido. Em seguida, a lista é impressa.