Claro! Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

// Função auxiliar para imprimir um vetor
void imprimirVetor(vector<int>& vetor) {
    for (int i = 0; i < vetor.size(); i++) {
        cout << vetor[i] << " ";
    }
    cout << endl;
}

// Função para ordenar um vetor usando o algoritmo Bubble Sort
void bubbleSort(vector<int>& vetor) {
    int tamanho = vetor.size();
    for (int i = 0; i < tamanho - 1; i++) {
        for (int j = 0; j < tamanho - i - 1; j++) {
            if (vetor[j] > vetor[j + 1]) {
                swap(vetor[j], vetor[j + 1]);
            }
        }
    }
}

// Função principal
int main() {
    int n;

    cout << "Digite o tamanho do vetor: ";
    cin >> n;

    vector<int> vetor(n);

    cout << "Digite os elementos do vetor:" << endl;
    for (int i = 0; i < n; i++) {
        cin >> vetor[i];
    }

    cout << "Vetor antes da ordenação: ";
    imprimirVetor(vetor);

    bubbleSort(vetor);

    cout << "Vetor após a ordenação: ";
    imprimirVetor(vetor);

    return 0;
}
```

Neste código, você poderá observar algumas características interessantes:

- Utilização de bibliotecas padrão do C++ como `iostream`, `vector` e `algorithm`.
- Função auxiliar `imprimirVetor` para exibir os elementos de um vetor na tela.
- Implementação do algoritmo Bubble Sort na função `bubbleSort`.
- Utilização de loops `for` aninhados para percorrer os elementos do vetor e realizar as comparações e trocas necessárias para ordená-lo.
- Recebimento do tamanho do vetor e dos elementos do usuário através da entrada padrão (`cin`).
- Exibição do vetor antes e depois da ordenação.

Espero que este código atenda às suas expectativas!