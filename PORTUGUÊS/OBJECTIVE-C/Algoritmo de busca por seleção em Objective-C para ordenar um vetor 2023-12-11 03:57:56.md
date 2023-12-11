Claro! Aqui está um código complexo em Objective-C que implementa um algoritmo de busca por seleção em um vetor de números inteiros:

```objective-c
#import <Foundation/Foundation.h>

void buscaPorSelecao(NSMutableArray *vetor) {
    for (int i = 0; i < vetor.count - 1; i++) {
        int indiceMinimo = i; // assume que o elemento atual é o menor

        for (int j = i + 1; j < vetor.count; j++) {
            // encontra o índice do menor elemento
            if ([vetor[j] intValue] < [vetor[indiceMinimo] intValue]) {
                indiceMinimo = j;
            }
        }

        // troca o elemento atual pelo menor elemento encontrado
        [vetor exchangeObjectAtIndex:i withObjectAtIndex:indiceMinimo];
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableArray *vetor = [[NSMutableArray alloc] initWithObjects:@5, @2, @8, @1, @9, nil];
        
        NSLog(@"Vetor original: %@", vetor);
        
        buscaPorSelecao(vetor);
        
        NSLog(@"Vetor ordenado: %@", vetor);
    }
    return 0;
}
```

Neste código, temos uma função chamada `buscaPorSelecao` que recebe um vetor como parâmetro e realiza um algoritmo de busca por seleção para ordenar o vetor em ordem crescente. O algoritmo funciona da seguinte maneira:

1. Percorremos o vetor a partir do primeiro elemento até o penúltimo elemento.
2. Para cada elemento, assumimos que ele é o menor e armazenamos o seu índice.
3. Em seguida, percorremos os elementos restantes no vetor e comparamos cada um com o elemento atualmente considerado como o menor.
4. Se encontrarmos um elemento menor do que o atualmente considerado como o menor, atualizamos o índice do menor elemento.
5. Após percorrer todos os elementos, trocamos o elemento atual pelo menor elemento encontrado.
6. Repetimos o processo para o próximo elemento.
7. Ao final do processo, o vetor estará ordenado em ordem crescente.

No `main`, criamos um vetor de exemplo com os números 5, 2, 8, 1 e 9. Imprimimos o vetor original e chamamos a função `buscaPorSelecao` para ordená-lo. Finalmente, imprimimos o vetor ordenado.

Espero que este código atenda às suas expectativas!