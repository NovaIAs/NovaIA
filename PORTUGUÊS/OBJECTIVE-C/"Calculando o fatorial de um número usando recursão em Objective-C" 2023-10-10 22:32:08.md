Aqui está um código em Objective-C que permite calcular o fatorial de um número usando recursão:

```objective-c
#import <Foundation/Foundation.h>

// Método recursivo para calcular o fatorial de um número
NSInteger calcularFatorial(NSInteger num) {
    if (num == 0) {
        return 1;
    } else {
        return num * calcularFatorial(num - 1);
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Solicita o número para o usuário
        NSFileHandle *input = [NSFileHandle fileHandleWithStandardInput];
        NSLog(@"Digite um número inteiro:");
        NSData *inputData = [input availableData];
        NSString *inputString = [[NSString alloc] initWithData:inputData encoding:NSUTF8StringEncoding];
        NSInteger numero = [inputString integerValue];
        
        // Calcula o fatorial
        NSInteger fatorial = calcularFatorial(numero);
        
        // Exibe o resultado
        NSLog(@"O fatorial de %ld é %ld", numero, fatorial);
    }
    return 0;
}
```

Neste código, usamos uma função chamada `calcularFatorial` que recebe um número inteiro como parâmetro e retorna o fatorial desse número. A função é implementada de forma recursiva, ou seja, ela chama a si mesma até chegar ao caso base, que é quando o número é igual a zero. Nesse caso, o fatorial é igual a 1. Caso contrário, a função calcula o fatorial do número menos um e multiplica pelo próprio número.

No método `main`, solicitamos um número inteiro ao usuário e o convertemos para um `NSInteger`. Em seguida, chamamos a função `calcularFatorial` passando o número digitado e armazenamos o resultado em uma variável chamada `fatorial`. Por fim, exibimos o resultado na tela utilizando `NSLog`.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.