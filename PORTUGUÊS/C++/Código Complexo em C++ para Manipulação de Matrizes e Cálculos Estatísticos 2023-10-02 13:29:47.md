Claro! Aqui está um exemplo de código complexo em C++ que envolve manipulação de matrizes e cálculos estatísticos:

```cpp
#include <iostream>
#include <cmath>
#include <vector>

// Função para calcular a média dos elementos de um vetor
double calcularMedia(const std::vector<double>& vetor) {
    double soma = 0;
    for (const auto& elemento : vetor) {
        soma += elemento;
    }
    return soma / vetor.size();
}

// Função para calcular o desvio padrão dos elementos de um vetor
double calcularDesvioPadrao(const std::vector<double>& vetor) {
    double media = calcularMedia(vetor);
    double somaQuadrados = 0;
    for (const auto& elemento : vetor) {
        somaQuadrados += pow(elemento - media, 2);
    }
    return sqrt(somaQuadrados / vetor.size());
}

int main() {
    // Criando uma matriz 3x3
    std::vector<std::vector<double>> matriz = {{1, 2, 3},
                                               {4, 5, 6},
                                               {7, 8, 9}};

    // Calculando a média e o desvio padrão de cada linha da matriz
    std::vector<double> medias;
    std::vector<double> desviosPadrao;
    for (const auto& linha : matriz) {
        medias.push_back(calcularMedia(linha));
        desviosPadrao.push_back(calcularDesvioPadrao(linha));
    }

    // Imprimindo os resultados
    std::cout << "Médias das linhas da matriz:" << std::endl;
    for (const auto& media : medias) {
        std::cout << media << " ";
    }
    std::cout << std::endl;

    std::cout << "Desvios padrão das linhas da matriz:" << std::endl;
    for (const auto& desvioPadrao : desviosPadrao) {
        std::cout << desvioPadrao << " ";
    }
    std::cout << std::endl;

    return 0;
}
```

Neste exemplo, o código cria uma matriz 3x3, calcula a média e o desvio padrão de cada linha da matriz e imprime os resultados na tela.