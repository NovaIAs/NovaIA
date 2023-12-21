Claro! Aqui está um exemplo de um código complexo em Objective-C que envolve o desenvolvimento de um sistema de gerenciamento de biblioteca. O código é extenso e possui comentários explicativos em cada seção.

```objective-c
#import <Foundation/Foundation.h>

// Classe para representar um livro
@interface Livro : NSObject

@property NSString *titulo;
@property NSString *autor;

@end

@implementation Livro

@end

// Classe para representar uma biblioteca
@interface Biblioteca : NSObject

@property NSMutableArray *livros;

// Método para adicionar um novo livro à biblioteca
- (void)adicionarLivro:(Livro *)livro;

// Método para buscar um livro pelo título
- (Livro *)buscarLivroPorTitulo:(NSString *)titulo;

@end

@implementation Biblioteca

- (instancetype)init {
    self = [super init];
    if (self) {
        _livros = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)adicionarLivro:(Livro *)livro {
    [self.livros addObject:livro];
    NSLog(@"Livro adicionado à biblioteca: %@, Autor: %@", livro.titulo, livro.autor);
}

- (Livro *)buscarLivroPorTitulo:(NSString *)titulo {
    for (Livro *livro in self.livros) {
        if ([livro.titulo isEqualToString:titulo]) {
            NSLog(@"Livro encontrado: %@, Autor: %@", livro.titulo, livro.autor);
            return livro;
        }
    }
    NSLog(@"Livro não encontrado");
    return nil;
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Cria uma instância da biblioteca
        Biblioteca *biblioteca = [[Biblioteca alloc] init];
        
        // Cria instâncias de livros e configura suas propriedades
        Livro *livro1 = [[Livro alloc] init];
        livro1.titulo = @"Dom Casmurro";
        livro1.autor = @"Machado de Assis";
        
        Livro *livro2 = [[Livro alloc] init];
        livro2.titulo = @"Grande Sertão: Veredas";
        livro2.autor = @"João Guimarães Rosa";
        
        Livro *livro3 = [[Livro alloc] init];
        livro3.titulo = @"Memórias Póstumas de Brás Cubas";
        livro3.autor = @"Machado de Assis";
        
        // Adiciona os livros à biblioteca
        [biblioteca adicionarLivro:livro1];
        [biblioteca adicionarLivro:livro2];
        [biblioteca adicionarLivro:livro3];
        
        // Busca um livro pelo título
        [biblioteca buscarLivroPorTitulo:@"Dom Casmurro"];
        
        // Busca um livro inexistente
        [biblioteca buscarLivroPorTitulo:@"O Senhor dos Anéis"];
        
    }
    return 0;
}
```

Neste código, temos duas classes: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com as propriedades de título e autor. A classe `Biblioteca` representa uma biblioteca, contendo um array de livros como propriedade. A biblioteca possui métodos para adicionar um novo livro e buscar um livro pelo título.

No `main`, criamos uma instância da biblioteca e algumas instâncias de livros. Em seguida, adicionamos os livros à biblioteca e realizamos uma busca por título, mostrando os resultados no console.