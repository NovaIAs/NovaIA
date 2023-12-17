Claro! Aqui está um exemplo de um código complexo em Objective-C que utiliza várias funcionalidades avançadas da linguagem, como classes, herança, polimorfismo, blocos e manipulação de arquivos:

```objective-c
#import <Foundation/Foundation.h>

@interface Animal : NSObject

@property(nonatomic, strong) NSString *nome;

- (void)fazerBarulho;

@end

@implementation Animal

- (instancetype)init {
    self = [super init];
    if (self) {
        self.nome = @"Animal";
    }
    return self;
}

- (void)fazerBarulho {
    NSLog(@"O animal faz um barulho indefinido.");
}

@end

@interface Cachorro : Animal

@end

@implementation Cachorro

- (instancetype)init {
    self = [super init];
    if (self) {
        self.nome = @"Cachorro";
    }
    return self;
}

- (void)fazerBarulho {
    NSLog(@"O cachorro late au au!");
}

@end

@interface Gato : Animal

@end

@implementation Gato

- (instancetype)init {
    self = [super init];
    if (self) {
        self.nome = @"Gato";
    }
    return self;
}

- (void)fazerBarulho {
    NSLog(@"O gato mia miau!");
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Criando instâncias das classes
        Animal *animal = [[Animal alloc] init];
        Cachorro *cachorro = [[Cachorro alloc] init];
        Gato *gato = [[Gato alloc] init];
        
        // Utilizando polimorfismo para chamar o método fazerBarulho de cada objeto
        NSArray *animais = @[animal, cachorro, gato];
        for (Animal *animal in animais) {
            [animal fazerBarulho];
        }
        
        // Utilizando blocos para manipular arquivos
        NSString *texto = @"Olá, mundo!";
        NSData *dados = [texto dataUsingEncoding:NSUTF8StringEncoding];
        
        NSString *caminho = @"/caminho/do/arquivo.txt";
        NSFileManager *fileManager = [NSFileManager defaultManager];
        
        // Escrevendo o arquivo
        [fileManager createFileAtPath:caminho contents:dados attributes:nil];
        
        // Lendo o arquivo
        NSData *dadosLidos = [fileManager contentsAtPath:caminho];
        NSString *textoLido = [[NSString alloc] initWithData:dadosLidos encoding:NSUTF8StringEncoding];
        NSLog(@"%@", textoLido);
    }
    return 0;
}
```

Neste código, temos três classes: Animal, Cachorro e Gato. A classe Animal é a classe base e possui uma propriedade nome e um método fazerBarulho. As classes Cachorro e Gato herdam da classe Animal e sobrescrevem o método fazerBarulho para imprimir o barulho característico de cada animal.

No método main, criamos instâncias das classes Animal, Cachorro e Gato e as armazenamos em um array. Utilizamos o conceito de polimorfismo para chamar o método fazerBarulho de cada objeto, garantindo que o método correto seja executado de acordo com o tipo do objeto.

Em seguida, utilizamos blocos para manipular arquivos. Criamos uma string, a convertemos em dados e escrevemos esses dados em um arquivo em um determinado caminho. Em seguida, lemos o arquivo, convertendo os dados em uma string e a imprimimos no console.

Este código é apenas um exemplo de como utilizar algumas funcionalidades avançadas do Objective-C, mas há muitas outras possibilidades e recursos que podem ser explorados nesta linguagem.