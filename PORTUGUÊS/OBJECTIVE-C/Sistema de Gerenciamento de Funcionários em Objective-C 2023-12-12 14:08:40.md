Claro! Aqui está um código complexo em Objective-C, escrito em português, que mostra um exemplo de um sistema de gerenciamento de funcionários em uma empresa:

```objective-c
#import <Foundation/Foundation.h>

@interface Funcionario : NSObject

@property NSString *nome;
@property int idade;
@property float salario;

- (instancetype)initWithNome:(NSString *)nome eIdade:(int)idade eSalario:(float)salario;
- (void)aumentarSalarioEmPercentual:(float)percentual;

@end

@implementation Funcionario

- (instancetype)initWithNome:(NSString *)nome eIdade:(int)idade eSalario:(float)salario {
    self = [super init];
    if (self) {
        self.nome = nome;
        self.idade = idade;
        self.salario = salario;
    }
    return self;
}

- (void)aumentarSalarioEmPercentual:(float)percentual {
    float aumento = self.salario * (percentual/100);
    self.salario += aumento;
}

@end

@interface Gerente : Funcionario

@property NSMutableArray *subordinados;

- (instancetype)initWithNome:(NSString *)nome eIdade:(int)idade eSalario:(float)salario eSubordinados:(NSMutableArray *)subordinados;
- (void)adicionarSubordinado:(Funcionario *)subordinado;
- (void)listarSubordinados;

@end

@implementation Gerente

- (instancetype)initWithNome:(NSString *)nome eIdade:(int)idade eSalario:(float)salario eSubordinados:(NSMutableArray *)subordinados {
    self = [super initWithNome:nome eIdade:idade eSalario:salario];
    if (self) {
        self.subordinados = subordinados;
    }
    return self;
}

- (void)adicionarSubordinado:(Funcionario *)subordinado {
    [self.subordinados addObject:subordinado];
}

- (void)listarSubordinados {
    for (Funcionario *subordinado in self.subordinados) {
        NSLog(@"Nome: %@, Idade: %d, Salário: %.2f", subordinado.nome, subordinado.idade, subordinado.salario);
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criando funcionários
        Funcionario *funcionario1 = [[Funcionario alloc] initWithNome:@"João" eIdade:25 eSalario:3000.0];
        Funcionario *funcionario2 = [[Funcionario alloc] initWithNome:@"Maria" eIdade:30 eSalario:4000.0];
        Funcionario *funcionario3 = [[Funcionario alloc] initWithNome:@"Pedro" eIdade:28 eSalario:3500.0];
        
        // Criando gerente
        Gerente *gerente = [[Gerente alloc] initWithNome:@"Carlos" eIdade:40 eSalario:6000.0 eSubordinados:@[funcionario1, funcionario2, funcionario3].mutableCopy];
        
        // Aumentando salário dos funcionários em 10%
        [funcionario1 aumentarSalarioEmPercentual:10];
        [funcionario2 aumentarSalarioEmPercentual:10];
        [funcionario3 aumentarSalarioEmPercentual:10];
        
        // Adicionando novo funcionário ao gerente
        Funcionario *funcionario4 = [[Funcionario alloc] initWithNome:@"Ana" eIdade:35 eSalario:4500.0];
        [gerente adicionarSubordinado:funcionario4];
        
        // Listando os funcionários do gerente
        [gerente listarSubordinados];
    }
    return 0;
}
```

Neste código, temos duas classes: `Funcionario` e `Gerente`. A classe `Funcionario` representa um funcionário com atributos como nome, idade e salário. A classe possui um método `initWithNome:eIdade:eSalario:` para inicializar o objeto e um método `aumentarSalarioEmPercentual:` para aumentar o salário em um determinado percentual.

Já a classe `Gerente` herda da classe `Funcionario` e adiciona o atributo `subordinados`, que é uma lista de funcionários. Ela possui um método `initWithNome:eIdade:eSalario:eSubordinados:` para inicializar o objeto com o nome, idade, salário e subordinados, um método `adicionarSubordinado:` para adicionar um novo funcionário à lista de subordinados e um método `listarSubordinados` para exibir as informações dos subordinados na saída.

No `main`, são criados quatro funcionários, incluindo um gerente, e seus salários são aumentados em 10%. Um novo funcionário é adicionado aos subordinados do gerente, e em seguida, a lista de subordinados é exibida na saída.

Espero que este exemplo atenda às suas expectativas!