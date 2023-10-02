Claro! Aqui está um código complexo em Objective-C para você:

```objective-c
#import <Foundation/Foundation.h>

@interface Person : NSObject

@property (nonatomic, strong) NSString *name;
@property (nonatomic, assign) NSInteger age;

- (instancetype)initWithName:(NSString *)name age:(NSInteger)age;
- (void)printDetails;

@end

@implementation Person

- (instancetype)initWithName:(NSString *)name age:(NSInteger)age {
    self = [super init];
    if (self) {
        _name = name;
        _age = age;
    }
    return self;
}

- (void)printDetails {
    NSLog(@"Name: %@, Age: %ld", self.name, (long)self.age);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Person *person1 = [[Person alloc] initWithName:@"John" age:25];
        [person1 printDetails];
        
        Person *person2 = [[Person alloc] initWithName:@"Jane" age:30];
        [person2 printDetails];
    }
    return 0;
}
```

Neste código, temos a definição de uma classe `Person` com propriedades para armazenar o nome e a idade de uma pessoa. A classe também possui um método de inicialização personalizado e um método para imprimir os detalhes da pessoa.

No `main`, criamos duas instâncias da classe `Person`, uma chamada `person1` com nome "John" e idade 25, e outra chamada `person2` com nome "Jane" e idade 30. Em seguida, chamamos o método `printDetails` em cada instância para imprimir os detalhes na saída.

Espero que este código lhe dê uma ideia de como usar Objective-C para criar classes e objetos.