Claro! Aqui está um exemplo de código em Objective-C que implementa um sistema de gerenciamento de funcionários em uma empresa:

```objective-c
#import <Foundation/Foundation.h>

// Classe Funcionario
@interface Funcionario : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, strong) NSString *cargo;
@property (nonatomic) int idade;
@property (nonatomic) float salario;

- (instancetype)initWithNome:(NSString *)nome cargo:(NSString *)cargo idade:(int)idade salario:(float)salario;
- (void)imprimirInformacoes;

@end

@implementation Funcionario

- (instancetype)initWithNome:(NSString *)nome cargo:(NSString *)cargo idade:(int)idade salario:(float)salario {
    self = [super init];
    if (self) {
        _nome = nome;
        _cargo = cargo;
        _idade = idade;
        _salario = salario;
    }
    return self;
}

- (void)imprimirInformacoes {
    NSLog(@"Nome: %@", self.nome);
    NSLog(@"Cargo: %@", self.cargo);
    NSLog(@"Idade: %d", self.idade);
    NSLog(@"Salario: %.2f", self.salario);
    NSLog(@"---------------");
}

@end

// Classe GerenciadorFuncionarios
@interface GerenciadorFuncionarios : NSObject

@property (nonatomic, strong) NSMutableArray *funcionarios;

- (void)adicionarFuncionario:(Funcionario *)funcionario;
- (void)removerFuncionario:(Funcionario *)funcionario;
- (void)imprimirRelatorio;

@end

@implementation GerenciadorFuncionarios

- (instancetype)init {
    self = [super init];
    if (self) {
        _funcionarios = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)adicionarFuncionario:(Funcionario *)funcionario {
    [self.funcionarios addObject:funcionario];
}

- (void)removerFuncionario:(Funcionario *)funcionario {
    [self.funcionarios removeObject:funcionario];
}

- (void)imprimirRelatorio {
    for (Funcionario *funcionario in self.funcionarios) {
        [funcionario imprimirInformacoes];
    }
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criando uma instância do gerenciador de funcionários
        GerenciadorFuncionarios *gerenciador = [[GerenciadorFuncionarios alloc] init];
        
        // Criando alguns funcionários
        Funcionario *funcionario1 = [[Funcionario alloc] initWithNome:@"João" cargo:@"Programador" idade:30 salario:5000.0];
        Funcionario *funcionario2 = [[Funcionario alloc] initWithNome:@"Maria" cargo:@"Designer" idade:28 salario:4000.0];
        Funcionario *funcionario3 = [[Funcionario alloc] initWithNome:@"Pedro" cargo:@"Gerente" idade:35 salario:8000.0];
        
        // Adicionando os funcionários ao gerenciador
        [gerenciador adicionarFuncionario:funcionario1];
        [gerenciador adicionarFuncionario:funcionario2];
        [gerenciador adicionarFuncionario:funcionario3];
        
        // Imprimindo o relatório dos funcionários
        [gerenciador imprimirRelatorio];
        
        // Removendo um funcionário
        [gerenciador removerFuncionario:funcionario2];
        
        // Imprimindo o relatório atualizado
        [gerenciador imprimirRelatorio];
    }
    return 0;
}
```

Este código em Objective-C demonstra a utilização de classes, propriedades, inicializadores e métodos em um sistema de gerenciamento de funcionários. A classe `Funcionario` representa um funcionário da empresa, com propriedades como nome, cargo, idade e salário. A classe `GerenciadorFuncionarios` é responsável por adicionar, remover e imprimir informações dos funcionários.

A função principal `main` cria uma instância do gerenciador de funcionários, cria alguns funcionários, adiciona-os ao gerenciador e imprime o relatório. Em seguida, um funcionário é removido e o relatório é impresso novamente para mostrar a atualização.

Espero que este código atenda às suas expectativas! Lembrando que você pode personalizá-lo conforme suas necessidades.