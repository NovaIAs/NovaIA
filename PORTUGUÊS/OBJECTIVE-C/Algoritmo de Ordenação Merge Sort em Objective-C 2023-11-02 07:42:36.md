Claro! Aqui está um exemplo de código em Objective-C que implementa um algoritmo de ordenação chamado Merge Sort:

```objective-c
#import <Foundation/Foundation.h>

@interface MergeSort : NSObject

+ (NSArray *)mergeSortArray:(NSArray *)array;

@end

@implementation MergeSort

+ (NSArray *)mergeSortArray:(NSArray *)array {
    if ([array count] <= 1) {
        return array;
    }
    
    NSInteger mid = [array count] / 2;
    NSArray *leftArray = [array subarrayWithRange:NSMakeRange(0, mid)];
    NSArray *rightArray = [array subarrayWithRange:NSMakeRange(mid, [array count] - mid)];
    
    NSArray *sortedLeftArray = [self mergeSortArray:leftArray];
    NSArray *sortedRightArray = [self mergeSortArray:rightArray];
    
    return [self mergeSortedArray:sortedLeftArray withArray:sortedRightArray];
}

+ (NSArray *)mergeSortedArray:(NSArray *)leftArray withArray:(NSArray *)rightArray {
    NSMutableArray *mergedArray = [NSMutableArray array];
    NSInteger leftIndex = 0;
    NSInteger rightIndex = 0;
    
    while (leftIndex < [leftArray count] && rightIndex < [rightArray count]) {
        if (leftArray[leftIndex] <= rightArray[rightIndex]) {
            [mergedArray addObject:leftArray[leftIndex]];
            leftIndex++;
        } else {
            [mergedArray addObject:rightArray[rightIndex]];
            rightIndex++;
        }
    }
    
    while (leftIndex < [leftArray count]) {
        [mergedArray addObject:leftArray[leftIndex]];
        leftIndex++;
    }
    
    while (rightIndex < [rightArray count]) {
        [mergedArray addObject:rightArray[rightIndex]];
        rightIndex++;
    }
    
    return [NSArray arrayWithArray:mergedArray];
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray *unsortedArray = @[@10, @3, @7, @1, @8, @6, @2, @5, @9, @4];
        
        NSLog(@"Unsorted Array: %@", unsortedArray);
        
        NSArray *sortedArray = [MergeSort mergeSortArray:unsortedArray];
        
        NSLog(@"Sorted Array: %@", sortedArray);
    }
    return 0;
}
```

Neste exemplo, implementamos uma classe `MergeSort` que contém dois métodos estáticos. O método `mergeSortArray:` implementa o algoritmo de ordenação Merge Sort. Ele recebe uma matriz de números inteiros desordenados e retorna a matriz ordenada. O método `mergeSortedArray:withArray:` é usado para mesclar e ordenar duas matrizes menores.

A classe `MergeSort` inclui um método `main` que é executado quando o programa é iniciado. Neste método, criamos uma matriz de exemplo `unsortedArray` com números desordenados. Em seguida, chamamos o método `mergeSortArray:` para ordenar a matriz e, finalmente, imprimimos a matriz ordenada usando `NSLog`.

O algoritmo Merge Sort divide a matriz original em duas metades, recursivamente ordena cada metade e, em seguida, mescla as duas metades ordenadas em uma matriz ordenada. Isso é feito repetidamente até que a matriz esteja completamente ordenada.

Espero que isso ajude a entender o código em Objective-C. Se você tiver alguma dúvida adicional, fique à vontade para perguntar!