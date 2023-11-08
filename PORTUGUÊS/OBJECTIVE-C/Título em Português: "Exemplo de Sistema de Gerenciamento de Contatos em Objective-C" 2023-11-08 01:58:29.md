Claro, posso gerar um código complexo em Objective-C para você. Vou criar um exemplo de um sistema de gerenciamento de contatos usando Objective-C. Este código incluirá classes para adicionar, listar e buscar contatos. Vou explicar o código à medida que o escrevo.

```objective-c
#import <Foundation/Foundation.h>

// Definindo a classe Contato
@interface Contato : NSObject

@property NSString *nome;
@property NSString *telefone;

- (instancetype)initWithNome:(NSString *)nome telefone:(NSString *)telefone;

@end

@implementation Contato

- (instancetype)initWithNome:(NSString *)nome telefone:(NSString *)telefone {
    self = [super init];
    if (self) {
        _nome = nome;
        _telefone = telefone;
    }
    return self;
}

@end

// Definindo a classe Agenda
@interface Agenda : NSObject

@property NSMutableArray *contatos;

- (void)adicionarContato:(Contato *)contato;
- (void)listarContatos;
- (Contato *)buscarContatoPorNome:(NSString *)nome;

@end

@implementation Agenda

- (instancetype)init {
    self = [super init];
    if (self) {
        _contatos = [NSMutableArray array];
    }
    return self;
}

- (void)adicionarContato:(Contato *)contato {
    [self.contatos addObject:contato];
}

- (void)listarContatos {
    NSLog(@"Lista de Contatos:");
    for (Contato *contato in self.contatos) {
        NSLog(@"Nome: %@, Telefone: %@", contato.nome, contato.telefone);
    }
}

- (Contato *)buscarContatoPorNome:(NSString *)nome {
    for (Contato *contato in self.contatos) {
        if ([contato.nome isEqualToString:nome]) {
            return contato;
        }
    }
    return nil;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Agenda *agenda = [[Agenda alloc] init];
        
        Contato *contato1 = [[Contato alloc] initWithNome:@"João" telefone:@"123-456-789"];
        Contato *contato2 = [[Contato alloc] initWithNome:@"Maria" telefone:@"987-654-321"];
        
        [agenda adicionarContato:contato1];
        [agenda adicionarContato:contato2];
        
        [agenda listarContatos];
        
        Contato *contatoBuscado = [agenda buscarContatoPorNome:@"Maria"];
        if (contatoBuscado) {
            NSLog(@"Contato encontrado: Nome: %@, Telefone: %@", contatoBuscado.nome, contatoBuscado.telefone);
        } else {
            NSLog(@"Contato não encontrado.");
        }
    }
    return 0;
}
```

Este código em Objective-C cria duas classes, `Contato` e `Agenda`. A classe `Contato` representa um contato com nome e telefone, enquanto a classe `Agenda` permite adicionar contatos, listar todos os contatos e buscar um contato pelo nome.

O código principal no método `main` cria uma instância da classe `Agenda`, adiciona dois contatos a ela, lista os contatos e faz uma busca por um contato específico.

Espero que este exemplo tenha demonstrado como criar um código complexo em Objective-C.