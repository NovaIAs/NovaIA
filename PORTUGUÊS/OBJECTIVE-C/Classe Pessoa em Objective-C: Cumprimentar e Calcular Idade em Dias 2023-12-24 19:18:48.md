Claro! Aqui está um exemplo de código complexo em Objective-C:

```objective-c
#import <Foundation/Foundation.h>

// Definição da classe "Pessoa"
@interface Pessoa : NSObject

// Propriedades
@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) NSInteger idade;

// Método para cumprimentar a pessoa
- (void)cumprimentar;

// Método para calcular a idade da pessoa em dias
- (NSInteger)calcularIdadeEmDias;

@end

@implementation Pessoa

// Implementação do método para cumprimentar a pessoa
- (void)cumprimentar
{
    NSLog(@"Olá, meu nome é %@ e tenho %ld anos.", self.nome, (long)self.idade);
}

// Implementação do método para calcular a idade da pessoa em dias
- (NSInteger)calcularIdadeEmDias
{
    NSCalendar *calendario = [NSCalendar currentCalendar];
    NSDate *dataAtual = [NSDate date];
    NSDateComponents *componentes = [calendario components:NSCalendarUnitYear | NSCalendarUnitMonth | NSCalendarUnitDay fromDate:[self obterDataDeNascimento] toDate:dataAtual options:0];
    
    NSInteger anosEmDias = [componentes year] * 365;
    NSInteger mesesEmDias = [componentes month] * 30;
    NSInteger dias = [componentes day];
    
    return anosEmDias + mesesEmDias + dias;
}

// Método auxiliar para obter a data de nascimento da pessoa
- (NSDate *)obterDataDeNascimento
{
    NSDateFormatter *formatador = [[NSDateFormatter alloc] init];
    [formatador setDateFormat:@"dd/MM/yyyy"];
    NSString *dataDeNascimentoString = @"01/01/2000"; // Exemplo de data de nascimento
    NSDate *dataDeNascimento = [formatador dateFromString:dataDeNascimentoString];
    
    return dataDeNascimento;
}

@end

// Função principal do programa
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criação de uma instância da classe "Pessoa"
        Pessoa *pessoa = [[Pessoa alloc] init];
        pessoa.nome = @"João";
        pessoa.idade = 30;
        
        // Chamada do método para cumprimentar a pessoa
        [pessoa cumprimentar];
        
        // Chamada do método para calcular a idade da pessoa em dias
        NSInteger idadeEmDias = [pessoa calcularIdadeEmDias];
        NSLog(@"Minha idade em dias é %ld.", (long)idadeEmDias);
    }
    return 0;
}
```

Neste exemplo, criamos uma classe `Pessoa` em Objective-C que possui propriedades para armazenar o nome e a idade de uma pessoa. A classe também possui dois métodos: `cumprimentar` e `calcularIdadeEmDias`.

O método `cumprimentar` simplesmente exibe uma mensagem de cumprimento, utilizando as propriedades `nome` e `idade`.

O método `calcularIdadeEmDias` utiliza a classe `NSCalendar` para obter a data atual e a data de nascimento da pessoa (no exemplo, fixada em 01/01/2000). Em seguida, é feito o cálculo da diferença entre essas datas, convertendo o resultado para o formato de dias.

No `main`, criamos uma instância da classe `Pessoa`, definimos o nome e a idade, e chamamos os métodos `cumprimentar` e `calcularIdadeEmDias`.

Esse código é apenas um exemplo e pode ser adaptado de acordo com as necessidades do projeto.