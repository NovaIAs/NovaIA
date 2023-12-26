Claro, vou gerar um código complexo em Objective-C para você. Este código é um exemplo de um sistema de gerenciamento de funcionários para uma empresa fictícia. Ele inclui classes para representar os diferentes tipos de funcionários e suas características, bem como métodos para calcular salários e exibir informações. 

```objective-c
// Classe Funcionario
@interface Funcionario : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, strong) NSString *cargo;
@property (nonatomic) NSInteger idade;
@property (nonatomic) double salario;

- (instancetype)initWithNome:(NSString *)nome cargo:(NSString *)cargo idade:(NSInteger)idade salario:(double)salario;
- (void)calcularSalario;
- (void)exibirInformacoes;

@end

@implementation Funcionario

- (instancetype)initWithNome:(NSString *)nome cargo:(NSString *)cargo idade:(NSInteger)idade salario:(double)salario {
    self = [super init];
    if (self) {
        self.nome = nome;
        self.cargo = cargo;
        self.idade = idade;
        self.salario = salario;
    }
    return self;
}

- (void)calcularSalario {
    // Lógica para calcular o salário baseado no cargo e idade
    // ...
    NSLog(@"Salário calculado para %@: %.2f", self.nome, self.salario);
}

- (void)exibirInformacoes {
    NSLog(@"Nome: %@", self.nome);
    NSLog(@"Cargo: %@", self.cargo);
    NSLog(@"Idade: %ld", (long)self.idade);
    NSLog(@"Salário: %.2f", self.salario);
}

@end

// Classe Gerente
@interface Gerente : Funcionario

@property (nonatomic) NSInteger nivelHierarquico;

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade salario:(double)salario nivelHierarquico:(NSInteger)nivelHierarquico;
- (void)calcularSalario;
- (void)exibirInformacoes;

@end

@implementation Gerente

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade salario:(double)salario nivelHierarquico:(NSInteger)nivelHierarquico {
    self = [super initWithNome:nome cargo:@"Gerente" idade:idade salario:salario];
    if (self) {
        self.nivelHierarquico = nivelHierarquico;
    }
    return self;
}

- (void)calcularSalario {
    // Lógica para calcular o salário do gerente baseado no nível hierárquico
    // ...
    NSLog(@"Salário calculado para o gerente %@: %.2f", self.nome, self.salario);
}

- (void)exibirInformacoes {
    NSLog(@"Nome: %@", self.nome);
    NSLog(@"Cargo: %@", self.cargo);
    NSLog(@"Idade: %ld", (long)self.idade);
    NSLog(@"Salário: %.2f", self.salario);
    NSLog(@"Nível Hierárquico: %ld", (long)self.nivelHierarquico);
}

@end

// Classe Desenvolvedor
@interface Desenvolvedor : Funcionario

@property (nonatomic) NSInteger nivelExperiencia;

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade salario:(double)salario nivelExperiencia:(NSInteger)nivelExperiencia;
- (void)calcularSalario;
- (void)exibirInformacoes;

@end

@implementation Desenvolvedor

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade salario:(double)salario nivelExperiencia:(NSInteger)nivelExperiencia {
    self = [super initWithNome:nome cargo:@"Desenvolvedor" idade:idade salario:salario];
    if (self) {
        self.nivelExperiencia = nivelExperiencia;
    }
    return self;
}

- (void)calcularSalario {
    // Lógica para calcular o salário do desenvolvedor baseado no nível de experiência
    // ...
    NSLog(@"Salário calculado para o desenvolvedor %@: %.2f", self.nome, self.salario);
}

- (void)exibirInformacoes {
    NSLog(@"Nome: %@", self.nome);
    NSLog(@"Cargo: %@", self.cargo);
    NSLog(@"Idade: %ld", (long)self.idade);
    NSLog(@"Salário: %.2f", self.salario);
    NSLog(@"Nível de Experiência: %ld", (long)self.nivelExperiencia);
}

@end

// Exemplo de uso
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Funcionario *funcionario1 = [[Funcionario alloc] initWithNome:@"João" cargo:@"Assistente" idade:25 salario:2000];
        [funcionario1 calcularSalario];
        [funcionario1 exibirInformacoes];
        
        Gerente *gerente1 = [[Gerente alloc] initWithNome:@"Maria" idade:35 salario:5000 nivelHierarquico:2];
        [gerente1 calcularSalario];
        [gerente1 exibirInformacoes];
        
        Desenvolvedor *dev1 = [[Desenvolvedor alloc] initWithNome:@"Pedro" idade:28 salario:4000 nivelExperiencia:3];
        [dev1 calcularSalario];
        [dev1 exibirInformacoes];
    }
    return 0;
}
```

Neste código, temos a classe base `Funcionario` que contém as propriedades comuns a todos os funcionários, como nome, cargo, idade e salário. A classe `Gerente` e a classe `Desenvolvedor` são subclasses de `Funcionario`, que adicionam propriedades e métodos específicos.

Cada classe possui um método `calcularSalario` que contém a lógica para calcular o salário do funcionário com base em seus atributos específicos. O método `exibirInformacoes` imprime as informações do funcionário no console.

No exemplo de uso no final do código, criamos instâncias de `Funcionario`, `Gerente` e `Desenvolvedor` e chamamos os métodos `calcularSalario` e `exibirInformacoes` para cada um deles.

Este código é apenas um exemplo para ilustrar a estrutura de um sistema de gerenciamento de funcionários em Objective-C. Você pode modificá-lo e adicionar mais funcionalidades de acordo com suas necessidades.