Claro! Aqui está um exemplo de código complexo em Objective-C que utiliza recursão para calcular o fatorial de um número:

```objective-c
#import <Foundation/Foundation.h>

// Função recursiva para calcular o fatorial de um número
NSInteger calcularFatorial(NSInteger numero) {
    if (numero == 0 || numero == 1) {
        return 1;
    } else {
        return numero * calcularFatorial(numero - 1);
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Solicita ao usuário um número para calcular o fatorial
        NSLog(@"Digite um número para calcular o fatorial:");
        NSInteger numero;
        scanf("%ld", &numero);
        
        // Chama a função calcularFatorial e exibe o resultado
        NSInteger resultado = calcularFatorial(numero);
        NSLog(@"O fatorial de %ld é %ld", numero, resultado);
    }
    return 0;
}
```

Neste código, a função `calcularFatorial` é responsável por receber um número como parâmetro e calcular o fatorial desse número utilizando recursão. A recursão é uma técnica em que uma função chama a si mesma repetidamente até atingir uma condição de parada.

No código, a função `calcularFatorial` verifica se o número é igual a 0 ou 1, caso seja verdadeiro, ela retorna 1 (pois 0! e 1! são iguais a 1). Caso contrário, ela chama a si mesma passando o número decrementado em 1, multiplicando-o pelo resultado dessa chamada.

No `main`, o programa solicita ao usuário um número, lê o valor digitado e chama a função `calcularFatorial` com esse número. O resultado é então exibido no console.

Espero que esse exemplo atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais ajuda, fique à vontade para perguntar.