Claro! Aqui está um exemplo de código em C++ que realiza a ordenação de um vetor utilizando o algoritmo Bubble Sort. O algoritmo percorre o vetor diversas vezes, comparando elementos adjacentes e trocando-os se estiverem na ordem errada, até que o vetor esteja completamente ordenado.

```cpp
#include <iostream>
using namespace std;

void bubbleSort(int vetor[], int tamanho) {
    for (int i = 0; i < tamanho - 1; i++) {
        for (int j = 0; j < tamanho - i - 1; j++) {
            if (vetor[j] > vetor[j + 1]) {
                int temp = vetor[j];
                vetor[j] = vetor[j + 1];
                vetor[j + 1] = temp;
            }
        }
    }
}

int main() {
    int vetor[] = {5, 3, 8, 1, 2};
    int tamanho = sizeof(vetor) / sizeof(vetor[0]);

    cout << "Vetor original: ";
    for (int i = 0; i < tamanho; i++) {
        cout << vetor[i] << " ";
    }
    cout << endl;

    bubbleSort(vetor, tamanho);

    cout << "Vetor ordenado: ";
    for (int i = 0; i < tamanho; i++) {
        cout << vetor[i] << " ";
    }
    cout << endl;

    return 0;
}
```

Neste código, a função `bubbleSort` recebe como parâmetros um vetor de inteiros e o tamanho desse vetor. Utilizando dois loops aninhados, o algoritmo compara cada elemento com seu próximo adjacente. Se o elemento atual for maior que o próximo, ocorre a troca dos valores. Esse processo se repete até que o vetor esteja completamente ordenado.

No `main`, é criado um vetor de exemplo `{5, 3, 8, 1, 2}` e seu tamanho é calculado utilizando a fórmula `sizeof(vetor) / sizeof(vetor[0])`. Os elementos do vetor são exibidos antes e depois da ordenação, utilizando o `cout`.

Esse é apenas um exemplo de código em C++ que utiliza o algoritmo Bubble Sort. Existem muitas outras formas de implementar algoritmos de ordenação, cada uma com suas vantagens e desvantagens.