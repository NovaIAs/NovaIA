Claro! Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <cstdlib>
#include <ctime>
using namespace std;

// Função para gerar números aleatórios
int gerarNumeroAleatorio(int min, int max) {
    return rand() % (max - min + 1) + min;
}

// Função para verificar se um número é primo
bool verificarPrimo(int numero) {
    if (numero <= 1) {
        return false;
    }

    for (int i = 2; i <= numero/2; i++) {
        if (numero % i == 0) {
            return false;
        }
    }

    return true;
}

// Função para calcular a média de um array
double calcularMedia(int array[], int tamanho) {
    int soma = 0;

    for (int i = 0; i < tamanho; i++) {
        soma += array[i];
    }

    return static_cast<double>(soma) / tamanho;
}

// Função principal
int main() {
    srand(static_cast<unsigned int>(time(0)));

    int tamanhoArray;
    cout << "Digite o tamanho do array: ";
    cin >> tamanhoArray;

    int array[tamanhoArray];
    for (int i = 0; i < tamanhoArray; i++) {
        array[i] = gerarNumeroAleatorio(1, 100);
    }

    cout << "Array gerado: ";
    for (int i = 0; i < tamanhoArray; i++) {
        cout << array[i] << " ";
    }
    cout << endl;

    double media = calcularMedia(array, tamanhoArray);
    cout << "A média do array é: " << media << endl;

    int countPrimos = 0;
    for (int i = 0; i < tamanhoArray; i++) {
        if (verificarPrimo(array[i])) {
            countPrimos++;
        }
    }

    cout << "Existem " << countPrimos << " números primos no array." << endl;

    return 0;
}
```

Este código em C++ gera um array de números aleatórios, calcula a média dos números e verifica a quantidade de números primos presentes no array.

Na função `gerarNumeroAleatorio()`, os números aleatórios são gerados dentro de um intervalo especificado. A função `verificarPrimo()` verifica se um número é primo ou não. A função `calcularMedia()` calcula a média dos valores de um array.

Na função principal `main()`, o usuário é solicitado a inserir o tamanho do array. Em seguida, o array é preenchido com números aleatórios usando a função `gerarNumeroAleatorio()`. Os números do array são exibidos na tela.

Em seguida, a média do array é calculada usando a função `calcularMedia()` e é exibida na tela. Por fim, a quantidade de números primos presentes no array é contada usando a função `verificarPrimo()` e também é exibida na tela.

Espero que este código atenda às suas expectativas!