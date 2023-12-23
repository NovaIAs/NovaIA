Certamente! Aqui está um código complexo em Objective-C que utiliza recursão para calcular o fatorial de um número:

```objective-c
#import <Foundation/Foundation.h>

// Função recursiva para calcular o fatorial
int calcularFatorial(int num) {
    if (num <= 1) {
        return 1;
    } else {
        return num * calcularFatorial(num - 1);
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Solicitar ao usuário um número para calcular o fatorial
        NSLog(@"Digite um número inteiro para calcular o fatorial:");
        int numero;
        scanf("%d", &numero);
        
        // Chamar a função recursiva para calcular o fatorial
        int resultado = calcularFatorial(numero);
        
        // Exibir o resultado
        NSLog(@"O fatorial de %d é %d", numero, resultado);
    }
    return 0;
}
```

Neste código, a função `calcularFatorial` recebe um número inteiro como parâmetro e retorna o fatorial desse número. A recursão é utilizada para chamar a função dentro dela mesma, decrementando o número em 1 a cada chamada, até que o número seja menor ou igual a 1. Nesse caso, o valor 1 é retornado. Caso contrário, o resultado é multiplicado pelo número atual e a função é chamada novamente com o número decrementado.

No `main`, o programa solicita ao usuário um número inteiro e armazena-o na variável `numero`. Em seguida, a função `calcularFatorial` é chamada passando esse número como argumento, e o resultado é armazenado na variável `resultado`. Por fim, o programa exibe o resultado utilizando o `NSLog`.

Esse código é apenas um exemplo de como utilizar recursão para calcular o fatorial de um número em Objective-C. É importante lembrar que a recursão pode levar a um consumo elevado de memória e tempo de execução, portanto, é necessário ter cuidado ao utilizá-la em situações reais.