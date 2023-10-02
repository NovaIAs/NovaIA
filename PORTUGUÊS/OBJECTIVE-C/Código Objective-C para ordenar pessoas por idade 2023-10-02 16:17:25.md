Claro! Aqui está um código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa: NSObject
@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;
@end

@implementation Pessoa
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableArray *pessoas = [NSMutableArray array];
        
        Pessoa *p1 = [[Pessoa alloc] init];
        p1.nome = @"João";
        p1.idade = 25;
        [pessoas addObject:p1];
        
        Pessoa *p2 = [[Pessoa alloc] init];
        p2.nome = @"Maria";
        p2.idade = 30;
        [pessoas addObject:p2];
        
        Pessoa *p3 = [[Pessoa alloc] init];
        p3.nome = @"Pedro";
        p3.idade = 28;
        [pessoas addObject:p3];
        
        NSSortDescriptor *sortDescriptor = [NSSortDescriptor sortDescriptorWithKey:@"idade" ascending:YES];
        NSArray *sortedPessoas = [pessoas sortedArrayUsingDescriptors:@[sortDescriptor]];
        
        for (Pessoa *pessoa in sortedPessoas) {
            NSLog(@"Nome: %@, Idade: %ld", pessoa.nome, (long)pessoa.idade);
        }
    }
    return 0;
}
```

Este código cria uma classe `Pessoa` com as propriedades `nome` e `idade`. Em seguida, cria três instâncias dessa classe e as adiciona a um `NSMutableArray`. O código então utiliza um `NSSortDescriptor` para ordenar as pessoas pelo campo `idade`, em ordem crescente. Por fim, o código percorre o array ordenado e imprime no console o nome e a idade de cada pessoa.