Claro! Aqui está um exemplo de código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

// Classe de exemplo para representar uma Pessoa
@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome eIdade:(NSInteger)idade;
- (void)apresentar;

@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome eIdade:(NSInteger)idade {
    self = [super init];
    if (self) {
        _nome = nome;
        _idade = idade;
    }
    return self;
}

- (void)apresentar {
    NSLog(@"Olá, meu nome é %@ e tenho %ld anos.", self.nome, (long)self.idade);
}

@end

// Classe de exemplo para representar uma Empresa
@interface Empresa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, strong) NSMutableArray *funcionarios;

- (instancetype)initWithNome:(NSString *)nome;
- (void)contratarFuncionario:(Pessoa *)funcionario;
- (void)listarFuncionarios;

@end

@implementation Empresa

- (instancetype)initWithNome:(NSString *)nome {
    self = [super init];
    if (self) {
        _nome = nome;
        _funcionarios = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)contratarFuncionario:(Pessoa *)funcionario {
    [self.funcionarios addObject:funcionario];
}

- (void)listarFuncionarios {
    NSLog(@"Funcionários da empresa %@:", self.nome);
    for (Pessoa *funcionario in self.funcionarios) {
        [funcionario apresentar];
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criando algumas instâncias de Pessoa
        Pessoa *pessoa1 = [[Pessoa alloc] initWithNome:@"João" eIdade:30];
        Pessoa *pessoa2 = [[Pessoa alloc] initWithNome:@"Maria" eIdade:25];
        Pessoa *pessoa3 = [[Pessoa alloc] initWithNome:@"Carlos" eIdade:35];
        
        // Criando uma instância de Empresa
        Empresa *empresa = [[Empresa alloc] initWithNome:@"ACME"];
        
        // Contratando os funcionários
        [empresa contratarFuncionario:pessoa1];
        [empresa contratarFuncionario:pessoa2];
        [empresa contratarFuncionario:pessoa3];
        
        // Listando os funcionários da empresa
        [empresa listarFuncionarios];
    }
    return 0;
}
```

Neste exemplo, criamos duas classes em Objective-C: `Pessoa` e `Empresa`. A classe `Pessoa` representa uma pessoa com nome e idade, e possui um método para se apresentar. A classe `Empresa` representa uma empresa com nome e uma lista de funcionários, e possui métodos para contratar funcionários e listar os funcionários contratados.

No método `main`, criamos algumas instâncias de `Pessoa`, uma instância de `Empresa` chamada "ACME" e contratamos os funcionários. Por fim, listamos os funcionários da empresa.

Esse código demonstra a estrutura básica de classes em Objective-C, a criação de instâncias, o uso de propriedades e métodos, além do conceito de composição de objetos.