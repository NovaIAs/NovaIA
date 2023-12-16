Certamente! Aqui está um exemplo de um código complexo em Objective-C:

```objective-c
// Classe para representar um carro
@interface Carro : NSObject

@property (nonatomic, strong) NSString *modelo;
@property (nonatomic, strong) NSString *marca;
@property (nonatomic, assign) int ano;
@property (nonatomic, assign) float velocidade;

- (instancetype)initWithModelo:(NSString *)modelo marca:(NSString *)marca ano:(int)ano velocidade:(float)velocidade;
- (void)acelerar;
- (void)frear;

@end

@implementation Carro

- (instancetype)initWithModelo:(NSString *)modelo marca:(NSString *)marca ano:(int)ano velocidade:(float)velocidade {
    self = [super init];
    if (self) {
        _modelo = modelo;
        _marca = marca;
        _ano = ano;
        _velocidade = velocidade;
    }
    return self;
}

- (void)acelerar {
    self.velocidade += 10;
    NSLog(@"O carro %@ da marca %@ acelerou para %.2f km/h", self.modelo, self.marca, self.velocidade);
}

- (void)frear {
    if (self.velocidade > 0) {
        self.velocidade -= 5;
        NSLog(@"O carro %@ da marca %@ freou para %.2f km/h", self.modelo, self.marca, self.velocidade);
    } else {
        NSLog(@"O carro %@ da marca %@ já está parado", self.modelo, self.marca);
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Carro *meuCarro = [[Carro alloc] initWithModelo:@"Sedan" marca:@"Ford" ano:2022 velocidade:0];
        
        NSLog(@"Informações iniciais do carro:");
        NSLog(@"Modelo: %@", meuCarro.modelo);
        NSLog(@"Marca: %@", meuCarro.marca);
        NSLog(@"Ano: %d", meuCarro.ano);
        NSLog(@"Velocidade: %.2f km/h", meuCarro.velocidade);
        
        [meuCarro acelerar];
        [meuCarro acelerar];
        [meuCarro frear];
        [meuCarro frear];
    }
    return 0;
}
```

Neste exemplo, criei uma classe `Carro` que possui quatro propriedades: `modelo`, `marca`, `ano` e `velocidade`. A classe também possui um inicializador `initWithModelo:marca:ano:velocidade:`, bem como métodos `acelerar` e `frear`. 

No método `main`, criei uma instância da classe `Carro`, configurei suas propriedades iniciais e demonstrei o uso dos métodos `acelerar` e `frear` através de mensagens de log.

Espero que este código atenda às suas necessidades!