Claro! Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <vector>

// Função para encontrar o maior número em um vetor
int encontrarMaiorNumero(std::vector<int> numeros) {
    int maior = numeros[0];
    
    for (int i = 1; i < numeros.size(); i++) {
        if (numeros[i] > maior) {
            maior = numeros[i];
        }
    }
    
    return maior;
}

// Função para calcular a média de um vetor de números
double calcularMedia(std::vector<int> numeros) {
    int soma = 0;
    
    for (int i = 0; i < numeros.size(); i++) {
        soma += numeros[i];
    }
    
    return static_cast<double>(soma) / numeros.size();
}

// Função principal
int main() {
    std::vector<int> numeros;
    int n;
    
    std::cout << "Digite a quantidade de números: ";
    std::cin >> n;
    
    for (int i = 0; i < n; i++) {
        int numero;
        std::cout << "Digite o número " << i + 1 << ": ";
        std::cin >> numero;
        numeros.push_back(numero);
    }

    int maiorNumero = encontrarMaiorNumero(numeros);
    double media = calcularMedia(numeros);
    
    std::cout << "O maior número é: " << maiorNumero << std::endl;
    std::cout << "A média dos números é: " << media << std::endl;
    
    return 0;
}
```

Este código é um exemplo de um programa que permite ao usuário digitar uma quantidade arbitrária de números e, em seguida, encontra o maior número e calcula a média dos números digitados.

A função `encontrarMaiorNumero` recebe um vetor de inteiros e percorre todos os elementos para encontrar o maior valor. A função `calcularMedia` também recebe um vetor de inteiros e calcula a média dos números.

No `main`, o programa solicita ao usuário a quantidade de números a serem digitados. Em seguida, ele percorre um loop para solicitar cada número e adicioná-lo ao vetor `numeros`. Depois disso, o programa chama as funções `encontrarMaiorNumero` e `calcularMedia` para obter o maior número e a média dos números, respectivamente. Esses resultados são então exibidos na saída padrão.

Espero que este código atenda às suas expectativas!