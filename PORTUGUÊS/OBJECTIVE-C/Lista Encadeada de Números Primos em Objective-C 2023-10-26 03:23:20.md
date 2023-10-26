Claro! Aqui está um código em Objective-C que cria uma lista encadeada de números primos:

```objective-c
#import <Foundation/Foundation.h>

@interface Node : NSObject

@property (nonatomic) NSInteger value;
@property (nonatomic, strong) Node *next;

@end

@implementation Node

@end

@interface LinkedList : NSObject

@property (nonatomic, strong) Node *head;

- (void)addPrimeNumber:(NSInteger)number;
- (void)printList;

@end

@implementation LinkedList

- (void)addPrimeNumber:(NSInteger)number {
    if (![self isPrime:number]) {
        return;
    }
    
    Node *newNode = [[Node alloc] init];
    newNode.value = number;
    
    if (self.head == nil) {
        self.head = newNode;
    } else {
        Node *current = self.head;
        while (current.next != nil) {
            current = current.next;
        }
        current.next = newNode;
    }
}

- (BOOL)isPrime:(NSInteger)number {
    if (number <= 1) {
        return NO;
    }
    
    for (NSInteger i = 2; i <= sqrt(number); i++) {
        if (number % i == 0) {
            return NO;
        }
    }
    
    return YES;
}

- (void)printList {
    Node *current = self.head;
    while (current != nil) {
        NSLog(@"%ld", (long)current.value);
        current = current.next;
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        LinkedList *list = [[LinkedList alloc] init];
        
        // Adiciona números primos à lista
        for (NSInteger i = 2; i <= 100; i++) {
            [list addPrimeNumber:i];
        }
        
        // Imprime a lista encadeada de números primos
        [list printList];
    }
    return 0;
}
```

Explicação:

1. Começamos importando a biblioteca Foundation para usar classes e métodos do Objective-C.

2. Em seguida, definimos a classe `Node`, que representa um nó na lista encadeada. Cada nó possui um valor (número primo) e uma referência para o próximo nó na lista.

3. A classe `LinkedList` é definida e possui uma propriedade `head` que representa a cabeça da lista encadeada.

4. O método `addPrimeNumber:` é implementado para adicionar um número primo à lista encadeada. Primeiro, é verificado se o número é primo usando o método `isPrime:`. Se for primo, um novo nó é criado com o valor passado como argumento. Se a lista estiver vazia, o novo nó se torna a cabeça da lista. Caso contrário, percorremos a lista até o último nó e adicionamos o novo nó no final.

5. O método `isPrime:` verifica se um número é primo. Se o número for menor ou igual a 1, não é primo. Caso contrário, percorremos os números de 2 até a raiz quadrada do número e verificamos se algum deles é divisor do número. Se encontrar algum divisor, o número não é primo.

6. O método `printList` é implementado para imprimir os valores da lista encadeada. Percorremos a lista começando pela cabeça e imprimimos o valor de cada nó.

7. No método `main`, criamos uma instância da classe `LinkedList` chamada `list`.

8. Em seguida, usamos um loop para adicionar os números primos de 2 a 100 à lista, usando o método `addPrimeNumber:`.

9. Por fim, chamamos o método `printList` para imprimir a lista encadeada de números primos.

Este código cria uma lista encadeada de números primos e imprime os valores. É uma implementação simples, mas demonstra o conceito de lista encadeada e a identificação de números primos.