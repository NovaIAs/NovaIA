Claro! Aqui está um exemplo de um código complexo em Objective-C que utiliza conceitos avançados de orientação a objetos e manipulação de arquivos:

```objective-c
#import <Foundation/Foundation.h>

@interface Pessoa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

- (instancetype)initWithNome:(NSString *)nome andIdade:(NSInteger)idade;
- (void)saudacao;

@end

@implementation Pessoa

- (instancetype)initWithNome:(NSString *)nome andIdade:(NSInteger)idade {
    self = [super init];
    if (self) {
        self.nome = nome;
        self.idade = idade;
    }
    return self;
}

- (void)saudacao {
    NSLog(@"Olá, meu nome é %@ e tenho %ld anos.", self.nome, (long)self.idade);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criar uma instância da classe Pessoa
        Pessoa *pessoa = [[Pessoa alloc] initWithNome:@"João" andIdade:25];
        
        // Chamar o método de saudação
        [pessoa saudacao];
        
        // Criar um arquivo para salvar informações da pessoa
        NSString *path = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
        NSString *filePath = [path stringByAppendingPathComponent:@"informacoes.txt"];
        
        // Criar um dicionário com as informações da pessoa
        NSDictionary *informacoes = @{
            @"Nome": pessoa.nome,
            @"Idade": @(pessoa.idade)
        };
        
        // Salvar as informações no arquivo
        [informacoes writeToFile:filePath atomically:YES];
        
        // Ler as informações do arquivo
        NSDictionary *informacoesLidas = [NSDictionary dictionaryWithContentsOfFile:filePath];
        
        // Imprimir as informações lidas
        NSLog(@"%@", informacoesLidas);
    }
    return 0;
}
```

Neste exemplo, criamos uma classe "Pessoa" que possui propriedades para o nome e idade da pessoa. Também implementamos um método de inicialização personalizado e um método de saudação. No método `main`, criamos uma instância da classe Pessoa, chamamos o método de saudação e salvamos as informações da pessoa em um arquivo chamado "informacoes.txt". Em seguida, lemos as informações do arquivo e as imprimimos no console.