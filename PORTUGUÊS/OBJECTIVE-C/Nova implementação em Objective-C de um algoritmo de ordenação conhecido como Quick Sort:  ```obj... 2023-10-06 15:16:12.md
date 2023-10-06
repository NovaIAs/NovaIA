Claro! Abaixo está um código em Objective-C que implementa um algoritmo de ordenação chamado Merge Sort.

```objective-c
#import <Foundation/Foundation.h>

// Função auxiliar para mesclar dois subarrays ordenados
void merge(NSMutableArray *array, NSUInteger left, NSUInteger middle, NSUInteger right)
{
    // Criar subarrays temporários para armazenar os valores ordenados
    NSMutableArray *leftArray = [[NSMutableArray alloc] init];
    NSMutableArray *rightArray = [[NSMutableArray alloc] init];
    
    // Preencher os subarrays temporários com os valores do array original
    for (NSUInteger i = left; i <= middle; i++) {
        [leftArray addObject:array[i]];
    }
    for (NSUInteger i = middle + 1; i <= right; i++) {
        [rightArray addObject:array[i]];
    }
    
    // Índices para percorrer os subarrays temporários
    NSUInteger leftIndex = 0;
    NSUInteger rightIndex = 0;
    
    // Índice para percorrer o array original
    NSUInteger currentIndex = left;
    
    // Mesclar os subarrays temporários em ordem crescente
    while (leftIndex < leftArray.count && rightIndex < rightArray.count) {
        if (leftArray[leftIndex] <= rightArray[rightIndex]) {
            array[currentIndex] = leftArray[leftIndex];
            leftIndex++;
        } else {
            array[currentIndex] = rightArray[rightIndex];
            rightIndex++;
        }
        currentIndex++;
    }
    
    // Adicionar os elementos restantes do subarray esquerdo, se houver algum
    while (leftIndex < leftArray.count) {
        array[currentIndex] = leftArray[leftIndex];
        leftIndex++;
        currentIndex++;
    }
    
    // Adicionar os elementos restantes do subarray direito, se houver algum
    while (rightIndex < rightArray.count) {
        array[currentIndex] = rightArray[rightIndex];
        rightIndex++;
        currentIndex++;
    }
}

// Função principal para dividir o array em subarrays menores e mesclá-los
void mergeSort(NSMutableArray *array, NSUInteger left, NSUInteger right)
{
    if (left < right) {
        NSUInteger middle = (left + right) / 2;
        
        // Ordenar o subarray esquerdo
        mergeSort(array, left, middle);
        
        // Ordenar o subarray direito
        mergeSort(array, middle + 1, right);
        
        // Mesclar os subarrays ordenados
        merge(array, left, middle, right);
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criar um array de exemplo
        NSMutableArray *array = [[NSMutableArray alloc] initWithArray:@[@9, @3, @6, @2, @1, @5, @8, @4, @7]];
        
        // Exibir o array original
        NSLog(@"Array original: %@", array);
        
        // Chamar a função de ordenação Merge Sort
        mergeSort(array, 0, array.count - 1);
        
        // Exibir o array ordenado
        NSLog(@"Array ordenado: %@", array);
    }
    return 0;
}
```

Este código implementa o algoritmo de ordenação Merge Sort em Objective-C. O Merge Sort é um algoritmo de divisão e conquista que divide o array em subarrays menores, ordena-os separadamente e, em seguida, mescla os subarrays ordenados para obter o array final ordenado.

A função `merge` é responsável por mesclar dois subarrays ordenados. Ela cria dois subarrays temporários, preenche-os com os valores do array original e, em seguida, mescla os subarrays em ordem crescente. Os elementos restantes de qualquer subarray são adicionados ao array original.

A função `mergeSort` é a função principal que divide o array em subarrays menores e chama recursivamente a si mesma para ordenar cada subarray. Em seguida, chama a função `merge` para mesclar os subarrays ordenados em um único array ordenado.

O código principal cria um array de exemplo, exibe o array original, chama a função `mergeSort` para ordenar o array e, por fim, exibe o array ordenado.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida, sinta-se à vontade para perguntar.