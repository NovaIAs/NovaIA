Claro! Aqui está um exemplo de um código complexo em C++ que realiza a ordenação de um vetor usando o algoritmo Merge Sort:

```cpp
#include <iostream>
using namespace std;

void merge(int arr[], int inicio, int meio, int fim) {
    int i, j, k;
    int n1 = meio - inicio + 1;
    int n2 = fim - meio;

    // Vetores temporários para armazenar as metades divididas
    int L[n1], R[n2];

    // Copiar os dados para os vetores temporários L[] e R[]
    for (i = 0; i < n1; i++)
        L[i] = arr[inicio + i];
    for (j = 0; j < n2; j++)
        R[j] = arr[meio + 1 + j];

    // Mesclar os vetores temporários de volta em arr[l..r]
    i = 0; // Índice inicial do primeiro subvetor
    j = 0; // Índice inicial do segundo subvetor
    k = inicio; // Índice inicial do subvetor mesclado
    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        } else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    // Copiar os elementos restantes do vetor L[], se houver algum
    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    // Copiar os elementos restantes do vetor R[], se houver algum
    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

void mergeSort(int arr[], int inicio, int fim) {
    if (inicio < fim) {
        // Encontrar o ponto médio para dividir o vetor em duas metades
        int meio = inicio + (fim - inicio) / 2;

        // Chamar recursivamente a função mergeSort() para as duas metades
        mergeSort(arr, inicio, meio);
        mergeSort(arr, meio + 1, fim);

        // Mesclar as duas metades ordenadas
        merge(arr, inicio, meio, fim);
    }
}

int main() {
    int arr[] = {12, 11, 13, 5, 6, 7};
    int tamanho = sizeof(arr) / sizeof(arr[0]);

    cout << "Vetor original: ";
    for (int i = 0; i < tamanho; i++)
        cout << arr[i] << " ";

    mergeSort(arr, 0, tamanho - 1);

    cout << "\nVetor ordenado: ";
    for (int i = 0; i < tamanho; i++)
        cout << arr[i] << " ";

    return 0;
}
```

Este código realiza a ordenação de um vetor usando o algoritmo Merge Sort. Primeiro, definimos duas funções: `merge()`, que mescla duas metades do vetor ordenado, e `mergeSort()`, que divide o vetor em metades recursivamente e chama a função `merge()` para mesclá-las.

Na função `merge()`, criamos dois vetores temporários para armazenar as metades divididas do vetor original. Em seguida, copiamos os dados para esses vetores temporários. Em seguida, mesclamos os vetores temporários de volta ao vetor original, comparando os elementos de ambos os vetores e colocando o menor no vetor original.

Na função `mergeSort()`, verificamos se o início é menor que o fim para garantir que ainda há elementos a serem ordenados. Em seguida, encontramos o ponto médio do vetor e chamamos recursivamente a função `mergeSort()` para as duas metades. Por fim, chamamos a função `merge()` para mesclar as duas metades ordenadas.

No `main()`, definimos um vetor de exemplo, exibimos o vetor original, chamamos a função `mergeSort()` para ordená-lo e exibimos o vetor ordenado.

Espero que este código complexo em C++ seja útil e atenda às suas expectativas! Lembre-se de que existem muitas maneiras de escrever um código complexo, então sinta-se à vontade para personalizá-lo de acordo com suas necessidades específicas.