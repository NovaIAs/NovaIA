Aqui está um exemplo de código em Objective-C que implementa um algoritmo de ordenação conhecido como Merge Sort. O Merge Sort é um algoritmo eficiente para ordenação de arrays que divide o array em pequenos pedaços, ordena cada pedaço e, em seguida, combina os pedaços ordenados para obter o array final:

```objective-c
#import <Foundation/Foundation.h>

void merge(int array[], int left, int middle, int right) {
    int i, j, k;
    int n1 = middle - left + 1;
    int n2 = right - middle;

    int leftArray[n1], rightArray[n2];

    for (i = 0; i < n1; i++)
        leftArray[i] = array[left + i];
    for (j = 0; j < n2; j++)
        rightArray[j] = array[middle + 1 + j];

    i = 0;
    j = 0;
    k = left;

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

    while (i < n1) {
        array[k] = leftArray[i];
        i++;
        k++;
    }

    while (j < n2) {
        array[k] = rightArray[j];
        j++;
        k++;
    }
}

void mergeSort(int array[], int left, int right) {
    if (left < right) {
        int middle = left + (right - left) / 2;

        mergeSort(array, left, middle);
        mergeSort(array, middle + 1, right);

        merge(array, left, middle, right);
    }
}

void printArray(int array[], int size) {
    for (int i = 0; i < size; i++)
        printf("%d ", array[i]);
    printf("\n");
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        int array[] = {9, 4, 2, 7, 1, 5, 8, 3, 6};
        int size = sizeof(array) / sizeof(array[0]);

        printf("Array antes da ordenação:\n");
        printArray(array, size);

        mergeSort(array, 0, size - 1);

        printf("Array após a ordenação:\n");
        printArray(array, size);
    }
    return 0;
}
```

Neste exemplo, o código começa incluindo a biblioteca Foundation.h, que é necessária para usar algumas funcionalidades do Objective-C. Em seguida, temos a definição das funções `merge` e `mergeSort`.

A função `merge` é responsável por mesclar duas partes do array. Ela recebe o array, os índices da parte esquerda (`left`), do meio (`middle`) e da parte direita (`right`) do array. Ela cria dois arrays temporários, `leftArray` e `rightArray`, para armazenar as partes esquerda e direita do array. Em seguida, ela compara os elementos dos dois arrays temporários e os mescla em ordem crescente no array original.

A função `mergeSort` é a função principal do algoritmo de ordenação. Ela recebe o array, os índices da parte esquerda (`left`) e da parte direita (`right`) do array. A função usa recursão para dividir o array em partes menores, chama a função `mergeSort` para ordenar cada parte e depois chama a função `merge` para mesclar as partes ordenadas.

A função `printArray` é uma função auxiliar para imprimir o array na tela.

No `main`, um array de exemplo é declarado e é calculado o tamanho do array. Então, o array é impresso antes da ordenação, a função `mergeSort` é chamada para ordenar o array e, por fim, o array é impresso novamente após a ordenação.

Espero que isso te ajude a entender e implementar o Merge Sort em Objective-C!