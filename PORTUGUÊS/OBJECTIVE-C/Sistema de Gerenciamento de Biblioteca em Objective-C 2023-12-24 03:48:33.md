Claro! Aqui está um código em Objective-C que cria um sistema de gerenciamento de biblioteca. O código é extenso e inclui várias classes e métodos para fornecer funcionalidades completas. Vamos lá:

```objective-c
// Classe Livro
@interface Livro : NSObject

@property (nonatomic, strong) NSString *titulo;
@property (nonatomic, strong) NSString *autor;
@property (nonatomic, strong) NSString *genero;

- (instancetype)initWithTitulo:(NSString *)titulo autor:(NSString *)autor genero:(NSString *)genero;

@end

@implementation Livro

- (instancetype)initWithTitulo:(NSString *)titulo autor:(NSString *)autor genero:(NSString *)genero {
    self = [super init];
    if (self) {
        _titulo = titulo;
        _autor = autor;
        _genero = genero;
    }
    return self;
}

@end

// Classe Biblioteca
@interface Biblioteca : NSObject

@property (nonatomic, strong) NSMutableArray *livros;

- (void)adicionarLivro:(Livro *)livro;
- (void)removerLivro:(Livro *)livro;
- (NSArray *)buscarLivrosPorAutor:(NSString *)autor;
- (NSArray *)buscarLivrosPorGenero:(NSString *)genero;

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
}

- (void)removerLivro:(Livro *)livro {
    [self.livros removeObject:livro];
}

- (NSArray *)buscarLivrosPorAutor:(NSString *)autor {
    NSMutableArray *livrosEncontrados = [[NSMutableArray alloc] init];
    for (Livro *livro in self.livros) {
        if ([livro.autor isEqualToString:autor]) {
            [livrosEncontrados addObject:livro];
        }
    }
    return [livrosEncontrados copy];
}

- (NSArray *)buscarLivrosPorGenero:(NSString *)genero {
    NSMutableArray *livrosEncontrados = [[NSMutableArray alloc] init];
    for (Livro *livro in self.livros) {
        if ([livro.genero isEqualToString:genero]) {
            [livrosEncontrados addObject:livro];
        }
    }
    return [livrosEncontrados copy];
}

@end

// Exemplo de uso do código
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Biblioteca *biblioteca = [[Biblioteca alloc] init];
        
        Livro *livro1 = [[Livro alloc] initWithTitulo:@"Dom Casmurro" autor:@"Machado de Assis" genero:@"Romance"];
        Livro *livro2 = [[Livro alloc] initWithTitulo:@"O Senhor dos Anéis" autor:@"J.R.R. Tolkien" genero:@"Fantasia"];
        Livro *livro3 = [[Livro alloc] initWithTitulo:@"1984" autor:@"George Orwell" genero:@"Ficção Científica"];
        
        [biblioteca adicionarLivro:livro1];
        [biblioteca adicionarLivro:livro2];
        [biblioteca adicionarLivro:livro3];
        
        NSArray *livrosPorAutor = [biblioteca buscarLivrosPorAutor:@"Machado de Assis"];
        NSLog(@"Livros por autor: %@", livrosPorAutor);
        
        NSArray *livrosPorGenero = [biblioteca buscarLivrosPorGenero:@"Fantasia"];
        NSLog(@"Livros por gênero: %@", livrosPorGenero);
    }
    return 0;
}
```

Neste código, temos duas classes: "Livro" e "Biblioteca". A classe "Livro" possui propriedades para título, autor e gênero, e um método de inicialização para configurar essas propriedades.

A classe "Biblioteca" possui um array de livros e métodos para adicionar, remover e buscar livros por autor ou gênero. A busca é feita percorrendo o array de livros e comparando as propriedades correspondentes.

No exemplo de uso no "main", criamos uma instância da classe "Biblioteca" e adicionamos alguns livros. Em seguida, fazemos buscas por autor e gênero e exibimos os resultados no console.

Espero que este código atenda às suas expectativas!