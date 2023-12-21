Claro! Aqui está um exemplo de código complexo em C++:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

// Função auxiliar para imprimir um vetor de inteiros
void imprimirVetor(const std::vector<int>& vetor) {
    std::cout << "Vetor: ";
    for (const auto& elemento : vetor) {
        std::cout << elemento << " ";
    }
    std::cout << std::endl;
}

// Função auxiliar para calcular a média de um vetor de inteiros
double calcularMedia(const std::vector<int>& vetor) {
    int soma = 0;
    for (const auto& elemento : vetor) {
        soma += elemento;
    }
    return static_cast<double>(soma) / vetor.size();
}

// Função auxiliar para calcular a mediana de um vetor de inteiros
double calcularMediana(const std::vector<int>& vetor) {
    std::vector<int> copiaVetor = vetor;
    std::sort(copiaVetor.begin(), copiaVetor.end());
    if (copiaVetor.size() % 2 == 0) {
        int indice1 = copiaVetor.size() / 2 - 1;
        int indice2 = copiaVetor.size() / 2;
        return (copiaVetor[indice1] + copiaVetor[indice2]) / 2.0;
    } else {
        int indice = copiaVetor.size() / 2;
        return copiaVetor[indice];
    }
}

// Função principal
int main() {
    std::vector<int> numeros = {5, 2, 9, 1, 7, 3, 8, 4, 6};

    std::cout << "Programa para cálculo de média e mediana de um vetor de inteiros" << std::endl;

    imprimirVetor(numeros);

    double media = calcularMedia(numeros);
    std::cout << "Média: " << media << std::endl;

    double mediana = calcularMediana(numeros);
    std::cout << "Mediana: " << mediana << std::endl;

    return 0;
}
```

Neste código, temos uma função auxiliar `imprimirVetor` que imprime os elementos de um vetor de inteiros. Também temos as funções auxiliares `calcularMedia` e `calcularMediana`, que calculam a média e a mediana de um vetor de inteiros, respectivamente.

Na função principal `main`, criamos um vetor `numeros` com alguns números inteiros. Em seguida, chamamos a função `imprimirVetor` para exibir os números na saída do console. Depois, calculamos a média e a mediana do vetor usando as funções `calcularMedia` e `calcularMediana`, respectivamente. Por fim, exibimos os resultados na saída do console.

Esse código é apenas um exemplo de um programa básico em C++ que calcula a média e a mediana de um vetor de inteiros. Você pode modificá-lo e adicionar mais funcionalidades conforme suas necessidades.