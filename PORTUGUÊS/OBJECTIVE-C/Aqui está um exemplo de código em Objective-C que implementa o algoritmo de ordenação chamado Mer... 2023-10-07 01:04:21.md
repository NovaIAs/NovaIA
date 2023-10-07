Claro! Aqui está um código em Objective-C que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo eficiente que divide a lista em duas metades, ordena cada metade recursivamente e, em seguida, combina as duas metades em uma única lista ordenada.

```objective-c
#import <Foundation/Foundation.h>

// Função para mesclar duas metades ordenadas de uma lista
void merge(NSMutableArray *arr, int left, int middle, int right) {
    int i, j, k;
    int n1 = middle - left + 1;
    int n2 = right - middle;
    
    // Cria arrays temporários para armazenar as metades esquerda e direita
    NSMutableArray *leftArray = [NSMutableArray arrayWithCapacity:n1];
    NSMutableArray *rightArray = [NSMutableArray arrayWithCapacity:n2];
    
    // Copia os elementos para os arrays temporários
    for (i = 0; i < n1; i++) {
        leftArray[i] = arr[left + i];
    }
    for (j = 0; j < n2; j++) {
        rightArray[j] = arr[middle + 1 + j];
    }
    
    // Mescla os arrays temporários de volta na lista original
    i = 0;
    j = 0;
    k = left;
    
    while (i < n1 && j < n2) {
        if (leftArray[i] <= rightArray[j]) {
            arr[k] = leftArray[i];
            i++;
        } else {
            arr[k] = rightArray[j];
            j++;
        }
        k++;
    }
    
    // Copia os elementos restantes do array esquerdo, se houver
    while (i < n1) {
        arr[k] = leftArray[i];
        i++;
        k++;
    }
    
    // Copia os elementos restantes do array direito, se houver
    while (j < n2) {
        arr[k] = rightArray[j];
        j++;
        k++;
    }
}

// Função para implementar o Merge Sort
void mergeSort(NSMutableArray *arr, int left, int right) {
    if (left < right) {
        int middle = (left + right) / 2;
        
        // Chama recursivamente o Merge Sort para a metade esquerda
        mergeSort(arr, left, middle);
        
        // Chama recursivamente o Merge Sort para a metade direita
        mergeSort(arr, middle + 1, right);
        
        // Mescla as duas metades ordenadas
        merge(arr, left, middle, right);
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableArray *arr = [NSMutableArray arrayWithArray:@[@12, @35, @87, @26, @9, @28, @7]];
        
        NSLog(@"Array não ordenado: %@", arr);
        
        int n = (int)[arr count];
        
        mergeSort(arr, 0, n - 1);
        
        NSLog(@"Array ordenado: %@", arr);
    }
    return 0;
}
```

Neste código, começamos declarando e implementando a função `merge`, que é responsável por mesclar duas metades ordenadas de uma lista. Em seguida, temos a função `mergeSort`, que implementa o algoritmo de Merge Sort. Finalmente, no `main`, criamos um exemplo de array não ordenado, chamamos a função `mergeSort` e imprimimos o array ordenado.

No Merge Sort, a lista é dividida recursivamente até que tenha apenas um elemento, e então as duas metades são mescladas em ordem. Esse processo continua até que a lista esteja completamente ordenada. O Merge Sort é um algoritmo eficiente com uma complexidade de tempo média e pior caso de O(n log n).

Espero que este código complexo em Objective-C tenha te ajudado a entender melhor o algoritmo de Merge Sort! Se você tiver mais alguma dúvida ou precisar de mais exemplos de código, é só me perguntar.