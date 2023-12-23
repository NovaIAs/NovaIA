Claro, aqui está um exemplo de um código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

// Classe Pessoa
@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade;
- (void)apresentar;

@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade {
    self = [super init];
    if (self) {
        _nome = nome;
        _idade = idade;
    }
    return self;
}

- (void)apresentar {
    NSLog(@"Olá, meu nome é %@ e tenho %ld anos.", self.nome, self.idade);
}

@end

// Classe Aluno herda de Pessoa
@interface Aluno : Pessoa

@property (nonatomic, strong) NSString *curso;

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade curso:(NSString *)curso;
- (void)apresentar;

@end

@implementation Aluno

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade curso:(NSString *)curso {
    self = [super initWithNome:nome idade:idade];
    if (self) {
        _curso = curso;
    }
    return self;
}

- (void)apresentar {
    NSLog(@"Olá, eu sou o aluno %@ do curso %@ e tenho %ld anos.", self.nome, self.curso, self.idade);
}

@end

// Classe Professor herda de Pessoa
@interface Professor : Pessoa

@property (nonatomic, strong) NSString *disciplina;

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade disciplina:(NSString *)disciplina;
- (void)apresentar;

@end

@implementation Professor

- (instancetype)initWithNome:(NSString *)nome idade:(NSInteger)idade disciplina:(NSString *)disciplina {
    self = [super initWithNome:nome idade:idade];
    if (self) {
        _disciplina = disciplina;
    }
    return self;
}

- (void)apresentar {
    NSLog(@"Olá, eu sou o professor %@ da disciplina %@ e tenho %ld anos.", self.nome, self.disciplina, self.idade);
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" idade:25];
        [pessoa apresentar];
        
        Aluno *aluno = [[Aluno alloc] initWithNome:@"Maria" idade:20 curso:@"Engenharia"];
        [aluno apresentar];
        
        Professor *professor = [[Professor alloc] initWithNome:@"Carlos" idade:40 disciplina:@"Matemática"];
        [professor apresentar];
    }
    return 0;
}
```

Neste exemplo de código, temos uma hierarquia de classes em Objective-C que representa pessoas, alunos e professores. A classe `Pessoa` é a classe base, que contém propriedades para nome e idade, além de um método `apresentar` para exibir uma mensagem de apresentação.

As classes `Aluno` e `Professor` herdam da classe `Pessoa` e adicionam propriedades específicas para curso (no caso do aluno) e disciplina (no caso do professor). Cada uma dessas classes também possui seu próprio método `apresentar`, que exibe uma mensagem personalizada.

Na função `main`, criamos instâncias de cada classe e chamamos o método `apresentar` para exibir as informações correspondentes.

Espero que este exemplo de código atenda às suas expectativas!