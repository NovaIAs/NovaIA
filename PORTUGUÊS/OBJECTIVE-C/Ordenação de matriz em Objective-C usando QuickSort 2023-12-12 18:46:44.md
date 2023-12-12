Claro! Aqui está um código complexo em Objective-C que gera uma matriz de números aleatórios e, em seguida, ordena essa matriz em ordem crescente usando o algoritmo de ordenação QuickSort:

#import <Foundation/Foundation.h>

void quickSort(NSMutableArray *vetor, int inicio, int fim) {
    
    if (inicio < fim) {
        int pivo = particionar(vetor, inicio, fim);
        
        quickSort(vetor, inicio, pivo - 1);
        quickSort(vetor, pivo + 1, fim);
    }
}

int particionar(NSMutableArray *vetor, int inicio, int fim) {
    int pivo = [[vetor objectAtIndex:fim] intValue];
    int i = inicio - 1;
    
    for (int j = inicio; j <= fim - 1; j++) {
        if ([[vetor objectAtIndex:j] intValue] < pivo) {
            i++;
            [vetor exchangeObjectAtIndex:i withObjectAtIndex:j];
        }
    }
    
    [vetor exchangeObjectAtIndex:i + 1 withObjectAtIndex:fim];
    return i + 1;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableArray *matriz = [[NSMutableArray alloc] initWithCapacity:10];
        
        for (int i = 0; i < 10; i++) {
            int numAleatorio = arc4random_uniform(100);
            [matriz addObject:[NSNumber numberWithInt:numAleatorio]];
        }
        
        NSLog(@"Matriz original: %@", matriz);
        
        quickSort(matriz, 0, (int)[matriz count] - 1);
        
        NSLog(@"Matriz ordenada: %@", matriz);
    }
    return 0;
}

Neste código, começamos importando a biblioteca Foundation para usar as classes e métodos do Objective-C. 

Em seguida, temos a função quickSort, que é uma implementação do algoritmo de ordenação QuickSort. Essa função recebe um NSMutableArray chamado "vetor", o índice de início (inicio) e o índice de fim (fim) do vetor a ser ordenado. Dentro dessa função, chamamos a função particionar para dividir o vetor em duas partes menores e ordená-las recursivamente.

A função particionar recebe o mesmo NSMutableArray "vetor", o índice de início (inicio) e o índice de fim (fim). Ela escolhe o último elemento do vetor como o pivô e rearranja os elementos de forma que os menores que o pivô fiquem à esquerda e os maiores fiquem à direita. O índice de retorno da função particionar é a posição correta do pivô no vetor.

Dentro do método main, criamos um NSMutableArray chamado "matriz" com capacidade para 10 elementos. Em seguida, usamos um loop para gerar 10 números aleatórios e adicioná-los à matriz.

Após isso, imprimimos a matriz original e chamamos a função quickSort para ordenar a matriz. Por fim, imprimimos a matriz ordenada.

Espero que este código complexo em Objective-C atenda às suas expectativas!