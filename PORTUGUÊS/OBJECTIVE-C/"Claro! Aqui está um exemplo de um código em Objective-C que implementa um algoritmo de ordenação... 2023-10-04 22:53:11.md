Claro! Aqui está um exemplo de um código em Objective-C que implementa um algoritmo de ordenação Merge Sort:

```objective-c
#import <Foundation/Foundation.h>

// Função para combinar dois subarrays ordenados
void merge(int array[], int left, int middle, int right) {
    int i, j, k;
    int n1 = middle - left + 1;
    int n2 = right - middle;
    
    // Cria arrays temporários
    int L[n1], R[n2];
    
    // Copia dados para os arrays temporários L[] e R[]
    for (i = 0; i < n1; i++)
        L[i] = array[left + i];
    for (j = 0; j < n2; j++)
        R[j] = array[middle + 1 + j];
    
    // Combina os arrays temporários de volta para o array original
    i = 0; // Índice inicial do primeiro subarray
    j = 0; // Índice inicial do segundo subarray
    k = left; // Índice inicial do subarray combinado
    
    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            array[k] = L[i];
            i++;
        } else {
            array[k] = R[j];
            j++;
        }
        k++;
    }
    
    // Copia os elementos restantes do subarray L[], se houver algum
    while (i < n1) {
        array[k] = L[i];
        i++;
        k++;
    }
    
    // Copia os elementos restantes do subarray R[], se houver algum
    while (j < n2) {
        array[k] = R[j];
        j++;
        k++;
    }
}

// Função principal para realizar o Merge Sort
void mergeSort(int array[], int left, int right) {
    if (left < right) {
        int middle = left + (right - left) / 2;
        
        // Ordena a primeira e a segunda metade do array
        mergeSort(array, left, middle);
        mergeSort(array, middle + 1, right);
        
        // Combina as metades ordenadas
        merge(array, left, middle, right);
    }
}

// Função auxiliar para imprimir o array ordenado
void printArray(int array[], int size) {
    for (int i = 0; i < size; i++)
        printf("%d ", array[i]);
    printf("\n");
}

// Função principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        int array[] = {12, 11, 13, 5, 6, 7};
        int size = sizeof(array) / sizeof(array[0]);
        
        printf("Array original: ");
        printArray(array, size);
        
        mergeSort(array, 0, size - 1);
        
        printf("Array ordenado: ");
        printArray(array, size);
    }
    return 0;
}
```

Neste exemplo, o código começa importando a biblioteca Foundation, necessária para utilizar as funcionalidades do Objective-C. Em seguida, há duas funções principais: `merge()` e `mergeSort()`. 

A função `merge()` é responsável por combinar dois subarrays ordenados. Ela recebe o array original, os índices de início, meio e fim dos subarrays e realiza a operação de combinação. Essa função é utilizada pela função `mergeSort()` para combinar os subarrays ordenados durante a etapa de divisão e conquista do algoritmo Merge Sort.

A função `mergeSort()` é a implementação do algoritmo Merge Sort em si. Ela recebe o array original, os índices de início e fim e realiza a ordenação recursiva do array dividindo-o em subarrays menores até que cada subarray tenha apenas um elemento. Em seguida, os subarrays são combinados usando a função `merge()`.

A função `printArray()` é uma função auxiliar para imprimir o array ordenado na tela.

Por fim, a função `main()` é a função principal do programa. Neste exemplo, um array com alguns valores desordenados é definido e impresso na tela antes da ordenação. Em seguida, a função `mergeSort()` é chamada para ordenar o array e, por fim, o array ordenado é impresso novamente na tela.