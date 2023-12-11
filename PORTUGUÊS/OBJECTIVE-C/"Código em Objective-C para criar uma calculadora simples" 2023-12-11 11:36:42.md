Claro! Aqui está um exemplo de um código em Objective-C para criar uma calculadora simples:

```objective-c
#import <Foundation/Foundation.h>
// Definição da interface Calculator
@interface Calculator : NSObject

@property (nonatomic, assign) float operand1;
@property (nonatomic, assign) float operand2;
@property (nonatomic, assign) char operator;

- (void)calculate;

@end

// Implementação da classe Calculator
@implementation Calculator

- (void)calculate {
    float result;
    
    switch (self.operator) {
        case '+':
            result = self.operand1 + self.operand2;
            NSLog(@"%.2f + %.2f = %.2f", self.operand1, self.operand2, result);
            break;
            
        case '-':
            result = self.operand1 - self.operand2;
            NSLog(@"%.2f - %.2f = %.2f", self.operand1, self.operand2, result);
            break;
            
        case '*':
            result = self.operand1 * self.operand2;
            NSLog(@"%.2f * %.2f = %.2f", self.operand1, self.operand2, result);
            break;
            
        case '/':
            if (self.operand2 != 0) {
                result = self.operand1 / self.operand2;
                NSLog(@"%.2f / %.2f = %.2f", self.operand1, self.operand2, result);
            } else {
                NSLog(@"Erro: Divisão por zero não é permitida.");
            }
            break;
            
        default:
            NSLog(@"Operador inválido.");
            break;
    }
}

@end

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Calculator *calculator = [[Calculator alloc] init];
        
        calculator.operand1 = 10.5;
        calculator.operand2 = 5.2;
        calculator.operator = '+';
        [calculator calculate];
        
        calculator.operand1 = 8.0;
        calculator.operand2 = 2.0;
        calculator.operator = '-';
        [calculator calculate];
        
        calculator.operand1 = 3.5;
        calculator.operand2 = 4.0;
        calculator.operator = '*';
        [calculator calculate];
        
        calculator.operand1 = 10.0;
        calculator.operand2 = 0.0;
        calculator.operator = '/';
        [calculator calculate];
    }
    return 0;
}
```

Neste código, criamos uma classe chamada `Calculator` que possui três propriedades: `operand1` (primeiro operando), `operand2` (segundo operando) e `operator` (operador aritmético). A classe também possui um método chamado `calculate` que realiza a operação aritmética com base nos operandos e no operador.

No método `calculate`, utilizamos um `switch` para determinar qual operação será realizada com base no operador. Em cada caso, realizamos a operação e exibimos o resultado no console, com duas casas decimais. Caso o operador seja inválido ou haja tentativa de dividir por zero, exibimos uma mensagem de erro apropriada.

Na função principal `main`, criamos uma instância da classe `Calculator` e definimos diferentes valores para os operandos e operador. Em seguida, chamamos o método `calculate` para realizar a operação e exibir o resultado.

Espero que este exemplo atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.