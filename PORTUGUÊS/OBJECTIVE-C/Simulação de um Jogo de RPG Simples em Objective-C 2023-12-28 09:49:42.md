Claro! Aqui está um código em Objective-C que realiza uma simulação de um jogo de RPG simples:

```objective-c
#import <Foundation/Foundation.h>

// Definição da classe Jogador
@interface Jogador : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) int nivel;
@property (nonatomic, assign) int pontosVida;
@property (nonatomic, assign) int pontosAtaque;

- (instancetype)initWithNome:(NSString *)nome nivel:(int)nivel pontosVida:(int)pontosVida pontosAtaque:(int)pontosAtaque;

@end

@implementation Jogador

- (instancetype)initWithNome:(NSString *)nome nivel:(int)nivel pontosVida:(int)pontosVida pontosAtaque:(int)pontosAtaque {
    self = [super init];
    if (self) {
        _nome = nome;
        _nivel = nivel;
        _pontosVida = pontosVida;
        _pontosAtaque = pontosAtaque;
    }
    return self;
}

@end

// Definição da classe Inimigo
@interface Inimigo : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, assign) int pontosVida;
@property (nonatomic, assign) int pontosAtaque;

- (instancetype)initWithNome:(NSString *)nome pontosVida:(int)pontosVida pontosAtaque:(int)pontosAtaque;

@end

@implementation Inimigo

- (instancetype)initWithNome:(NSString *)nome pontosVida:(int)pontosVida pontosAtaque:(int)pontosAtaque {
    self = [super init];
    if (self) {
        _nome = nome;
        _pontosVida = pontosVida;
        _pontosAtaque = pontosAtaque;
    }
    return self;
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Jogador *jogador = [[Jogador alloc] initWithNome:@"Herói" nivel:1 pontosVida:100 pontosAtaque:10];
        
        Inimigo *inimigo = [[Inimigo alloc] initWithNome:@"Monstro" pontosVida:50 pontosAtaque:5];
        
        NSLog(@"Batalha iniciada! %@ vs %@", jogador.nome, inimigo.nome);
        
        while (jogador.pontosVida > 0 && inimigo.pontosVida > 0) {
            // Jogador ataca o inimigo
            inimigo.pontosVida -= jogador.pontosAtaque;
            NSLog(@"%@ atacou %@ e causou %d de dano", jogador.nome, inimigo.nome, jogador.pontosAtaque);
            
            // Verifica se o inimigo foi derrotado
            if (inimigo.pontosVida <= 0) {
                NSLog(@"%@ derrotou %@!", jogador.nome, inimigo.nome);
                break;
            }
            
            // Inimigo ataca o jogador
            jogador.pontosVida -= inimigo.pontosAtaque;
            NSLog(@"%@ atacou %@ e causou %d de dano", inimigo.nome, jogador.nome, inimigo.pontosAtaque);
            
            // Verifica se o jogador foi derrotado
            if (jogador.pontosVida <= 0) {
                NSLog(@"%@ derrotou %@!", inimigo.nome, jogador.nome);
                break;
            }
        }
        
        NSLog(@"Fim da batalha!");
    }
    return 0;
}
```

Neste código, temos as classes `Jogador` e `Inimigo`, que representam os personagens do jogo. A classe `Jogador` possui propriedades como nome, nível, pontos de vida e pontos de ataque. A classe `Inimigo` possui propriedades semelhantes.

Na função principal (`main`), é criado um objeto jogador e um objeto inimigo. Em seguida, ocorre uma batalha entre eles, onde o jogador e o inimigo se atacam alternadamente até que um deles seja derrotado.

O código utiliza logs (`NSLog`) para exibir informações sobre a batalha no console.

Espero que esse código atenda às suas expectativas!