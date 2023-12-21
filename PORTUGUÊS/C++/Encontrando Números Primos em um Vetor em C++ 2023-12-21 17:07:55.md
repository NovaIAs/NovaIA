Claro! Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <vector>

// Função para verificar se um número é primo
bool ehPrimo(int numero) {
    if (numero <= 1) {
        return false;
    }

    for (int i = 2; i <= numero / 2; i++) {
        if (numero % i == 0) {
            return false;
        }
    }

    return true;
}

// Função para encontrar os números primos em um vetor
std::vector<int> encontrarPrimos(const std::vector<int>& vetor) {
    std::vector<int> primos;

    for (int numero : vetor) {
        if (ehPrimo(numero)) {
            primos.push_back(numero);
        }
    }

    return primos;
}

int main() {
    // Criando um vetor com números aleatórios
    std::vector<int> numeros = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 };

    // Encontrando os números primos no vetor
    std::vector<int> primos = encontrarPrimos(numeros);

    // Imprimindo os números primos encontrados
    std::cout << "Números primos encontrados: ";
    for (int numero : primos) {
        std::cout << numero << " ";
    }
    std::cout << std::endl;

    return 0;
}
```

Neste exemplo, temos uma função `ehPrimo` que verifica se um número é primo ou não. Em seguida, temos a função `encontrarPrimos`, que recebe um vetor de números e retorna um novo vetor contendo apenas os números primos do vetor original.

No `main`, criamos um vetor com alguns números aleatórios e chamamos a função `encontrarPrimos` passando esse vetor. Em seguida, imprimimos os números primos encontrados na saída padrão.

Esse código é apenas um exemplo, mas é um exemplo de como você pode usar funções e vetores em C++. É importante destacar que a complexidade de um código não está necessariamente relacionada ao seu tamanho, mas sim à sua lógica e à quantidade de recursos que ele utiliza.