Claro! Aqui está um código em Objective-C que implementa um algoritmo de ordenação conhecido como Bubble Sort. O Bubble Sort é um algoritmo simples, mas eficiente para ordenar uma lista de elementos.

```objective-c
#import <Foundation/Foundation.h>

void bubbleSort(NSMutableArray *array) {
    NSInteger n = [array count];
    
    for (NSInteger i = 0; i < n - 1; i++) {
        for (NSInteger j = 0; j < n - i - 1; j++) {
            if ([array[j] compare:array[j + 1]] == NSOrderedDescending) {
                [array exchangeObjectAtIndex:j withObjectAtIndex:j + 1];
            }
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableArray *numbers = [NSMutableArray arrayWithArray:@[@5, @3, @8, @2, @1]];
        
        NSLog(@"Array antes da ordenação: %@", numbers);
        
        bubbleSort(numbers);
        
        NSLog(@"Array após a ordenação: %@", numbers);
    }
    return 0;
}
```

Neste código em Objective-C, começo importando a biblioteca Foundation que é necessária para usar a classe `NSMutableArray` e alguns métodos como `NSOrderedDescending` e `exchangeObjectAtIndex`. 

Em seguida, temos a função `bubbleSort`, que recebe um NSMutableArray como parâmetro e implementa o algoritmo Bubble Sort. A função usa dois loops aninhados para percorrer a lista e comparar elementos adjacentes. Se um elemento for maior do que o próximo, eles são trocados de posição usando o método `exchangeObjectAtIndex`.

Na função `main`, crio um NSMutableArray chamado `numbers` com uma lista de números desordenados. Em seguida, imprimo a lista antes da ordenação usando `NSLog`. 

Chamo a função `bubbleSort` passando o array `numbers` como parâmetro para ordená-lo. Por fim, imprimo a lista ordenada usando `NSLog` novamente.

Ao executar o código, você verá a lista antes e depois da ordenação. O Bubble Sort é um algoritmo de ordenação simples, mas não é eficiente para listas muito grandes, pois possui uma complexidade de tempo de O(n^2). No entanto, ele é útil para fins educacionais e para listas pequenas.