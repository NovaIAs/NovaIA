Claro! Aqui está um código complexo em Objective-C, que envolve o conceito de herança, polimorfismo e manipulação de arrays. 

```objective-c
#import <Foundation/Foundation.h>

// Classe base Animal
@interface Animal : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (void)fazerBarulho;

@end

@implementation Animal

- (void)fazerBarulho {
    NSLog(@"O animal está fazendo barulho!");
}

@end

// Subclasse Cachorro
@interface Cachorro : Animal

- (void)fazerBarulho;

@end

@implementation Cachorro

- (void)fazerBarulho {
    NSLog(@"O cachorro está latindo!");
}

@end

// Subclasse Gato
@interface Gato : Animal

- (void)fazerBarulho;

@end

@implementation Gato

- (void)fazerBarulho {
    NSLog(@"O gato está miando!");
}

@end

// Classe principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Criação de uma instância de Animal
        Animal *animal = [[Animal alloc] init];
        animal.nome = @"Animal Genérico";
        animal.idade = 5;
        [animal fazerBarulho];
        
        // Criação de uma instância de Cachorro
        Cachorro *cachorro = [[Cachorro alloc] init];
        cachorro.nome = @"Pluto";
        cachorro.idade = 3;
        [cachorro fazerBarulho];
        
        // Criação de uma instância de Gato
        Gato *gato = [[Gato alloc] init];
        gato.nome = @"Tom";
        gato.idade = 2;
        [gato fazerBarulho];
        
        // Array de animais
        NSArray *animais = @[animal, cachorro, gato];
        
        // Iteração pelo array e chamada do método fazerBarulho
        for (Animal *animal in animais) {
            NSLog(@"Nome: %@, Idade: %ld", animal.nome, (long)animal.idade);
            [animal fazerBarulho];
        }
    }
    return 0;
}
```

Neste código, temos uma classe base chamada "Animal" que possui duas propriedades: "nome" e "idade". Também possui um método chamado "fazerBarulho", que no caso da classe base, apenas imprime uma mensagem genérica.

Em seguida, temos duas subclasses: "Cachorro" e "Gato". Cada uma dessas subclasses sobrescreve o método "fazerBarulho" para imprimir uma mensagem específica para cada animal.

Na função principal, criamos instâncias das classes "Animal", "Cachorro" e "Gato" e atribuímos valores às suas propriedades. Em seguida, chamamos o método "fazerBarulho" para cada uma delas.

Também criamos um array chamado "animais" e adicionamos as instâncias criadas anteriormente. Em seguida, iteramos pelo array e chamamos o método "fazerBarulho" para cada animal, mostrando seus nomes e idades.

Espero que este código complexo em Objective-C atenda às suas expectativas e demonstre algumas das funcionalidades da linguagem. Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição para ajudar!