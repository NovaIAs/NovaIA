Claro! Aqui está um exemplo de um código complexo em C++ que utiliza algoritmos de ordenação para organizar um vetor de números inteiros em ordem crescente. O código utiliza o algoritmo Quicksort, um dos algoritmos mais eficientes para ordenação.

```cpp
#include <iostream>
using namespace std;

void quicksort(int vetor[], int inicio, int fim) {
    int i, j, pivo, aux;
    i = inicio;
    j = fim;
    pivo = vetor[(inicio + fim) / 2];
    
    while (i <= j) {
        while (vetor[i] < pivo) {
            i++;
        }
        while (vetor[j] > pivo) {
            j--;
        }
        if (i <= j) {
            aux = vetor[i];
            vetor[i] = vetor[j];
            vetor[j] = aux;
            i++;
            j--;
        }
    }
    
    if (inicio < j) {
        quicksort(vetor, inicio, j);
    }
    if (i < fim) {
        quicksort(vetor, i, fim);
    }
}

int main() {
    int tamanho, i;
    
    cout << "Digite o tamanho do vetor: ";
    cin >> tamanho;
    
    int vetor[tamanho];
    
    cout << "Digite os elementos do vetor:" << endl;
    for (i = 0; i < tamanho; i++) {
        cin >> vetor[i];
    }
    
    quicksort(vetor, 0, tamanho - 1);
    
    cout << "Vetor ordenado em ordem crescente:" << endl;
    for (i = 0; i < tamanho; i++) {
        cout << vetor[i] << " ";
    }
    cout << endl;
    
    return 0;
}
```

Neste código, a função `quicksort` implementa o algoritmo de ordenação Quicksort. Ela recebe como parâmetros um vetor de inteiros, o índice de início e o índice de fim do trecho a ser ordenado. Dentro da função, é escolhido um elemento do vetor como pivô e os elementos são rearranjados de forma que os elementos menores que o pivô fiquem à sua esquerda e os elementos maiores fiquem à sua direita. Essa operação é repetida recursivamente para os subvetores à esquerda e à direita do pivô até que todo o vetor esteja ordenado.

No trecho `main`, o usuário insere o tamanho do vetor e em seguida os elementos do vetor. Após isso, a função `quicksort` é chamada para ordenar o vetor. Por fim, o vetor ordenado é exibido na tela.