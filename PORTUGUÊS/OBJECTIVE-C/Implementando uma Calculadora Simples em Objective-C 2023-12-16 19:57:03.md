Claro! Aqui está um exemplo de um código em Objective-C que implementa uma calculadora simples com funcionalidade de adição, subtração, multiplicação e divisão:

```objective-c
#import <Foundation/Foundation.h>

@interface Calculadora : NSObject

@property (nonatomic, assign) float valorAtual;

- (void)somar:(float)numero;
- (void)subtrair:(float)numero;
- (void)multiplicarPor:(float)numero;
- (void)dividirPor:(float)numero;
- (float)obterResultado;

@end

@implementation Calculadora

- (instancetype)init {
    self = [super init];
    if (self) {
        _valorAtual = 0;
    }
    return self;
}

- (void)somar:(float)numero {
    _valorAtual += numero;
}

- (void)subtrair:(float)numero {
    _valorAtual -= numero;
}

- (void)multiplicarPor:(float)numero {
    _valorAtual *= numero;
}

- (void)dividirPor:(float)numero {
    if (numero != 0) {
        _valorAtual /= numero;
    } else {
        NSLog(@"Não é possível dividir por zero");
    }
}

- (float)obterResultado {
    return _valorAtual;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Calculadora *calculadora = [[Calculadora alloc] init];
        
        [calculadora somar:10];
        [calculadora subtrair:5];
        [calculadora multiplicarPor:2];
        [calculadora dividirPor:4];
        
        float resultado = [calculadora obterResultado];
        NSLog(@"O resultado é: %.2f", resultado);
    }
    return 0;
}
```

Neste exemplo, a classe `Calculadora` é criada com uma propriedade `valorAtual` para armazenar o resultado atual das operações. Os métodos `somar`, `subtrair`, `multiplicarPor` e `dividirPor` são implementados para realizar as operações matemáticas básicas. O método `obterResultado` retorna o valor atual da calculadora.

No `main`, um objeto `Calculadora` é criado e algumas operações são realizadas: somar 10, subtrair 5, multiplicar por 2 e dividir por 4. O resultado final é exibido no console.

Este código é apenas um exemplo básico para demonstrar como uma calculadora simples pode ser implementada em Objective-C. Você pode expandir e modificar esse código de acordo com suas necessidades.