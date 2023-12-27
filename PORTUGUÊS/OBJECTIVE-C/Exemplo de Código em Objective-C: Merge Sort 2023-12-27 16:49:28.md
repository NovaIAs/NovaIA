Claro! Aqui está um exemplo de código em Objective-C que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo eficiente para ordenar um array de elementos.

```objective-c
#import <Foundation/Foundation.h>

// Função para fundir dois subarrays
void merge(int array[], int left, int middle, int right) {
    int i, j, k;
    int n1 = middle - left + 1;
    int n2 = right - middle;
    
    // Criar arrays temporários para armazenar os subarrays
    int leftArray[n1], rightArray[n2];
    
    // Copiar os dados para os arrays temporários
    for (i = 0; i < n1; i++)
        leftArray[i] = array[left + i];
    for (j = 0; j < n2; j++)
        rightArray[j] = array[middle + 1 + j];
    
    // Mesclar os subarrays de volta no array principal
    i = 0; // Índice inicial do primeiro subarray
    j = 0; // Índice inicial do segundo subarray
    k = left; // Índice inicial do array mesclado
    
    while (i < n1 && j < n2) {
        if (leftArray[i] <= rightArray[j]) {
            array[k] = leftArray[i];
            i++;
        } else {
            array[k] = rightArray[j];
            j++;
        }
        k++;
    }
    
    // Copiar os elementos restantes do subarray esquerdo, se houver algum
    while (i < n1) {
        array[k] = leftArray[i];
        i++;
        k++;
    }
    
    // Copiar os elementos restantes do subarray direito, se houver algum
    while (j < n2) {
        array[k] = rightArray[j];
        j++;
        k++;
    }
}

// Função para implementar o Merge Sort
void mergeSort(int array[], int left, int right) {
    if (left < right) {
        int middle = left + (right - left) / 2;
        
        // Ordenar recursivamente os subarrays esquerdo e direito
        mergeSort(array, left, middle);
        mergeSort(array, middle + 1, right);
        
        // Fundir os subarrays ordenados
        merge(array, left, middle, right);
    }
}

// Função para imprimir o array ordenado
void printArray(int array[], int size) {
    int i;
    for (i = 0; i < size; i++)
        printf("%d ", array[i]);
    printf("\n");
}

int main() {
    @autoreleasepool {
        int array[] = {64, 34, 25, 12, 22, 11, 90};
        int size = sizeof(array) / sizeof(array[0]);
        
        printf("Array original:\n");
        printArray(array, size);
        
        mergeSort(array, 0, size - 1);
        
        printf("Array ordenado:\n");
        printArray(array, size);
    }
    return 0;
}
```

Nesse código, a função `mergeSort` é responsável por dividir o array em subarrays menores e chamar recursivamente a função `mergeSort` para ordená-los. A função `merge` é responsável por fundir os subarrays ordenados em um único array ordenado.

No `main`, um array de exemplo é criado e impresso antes e depois da ordenação usando as funções `printArray`. O resultado final é um array ordenado pelo algoritmo Merge Sort.