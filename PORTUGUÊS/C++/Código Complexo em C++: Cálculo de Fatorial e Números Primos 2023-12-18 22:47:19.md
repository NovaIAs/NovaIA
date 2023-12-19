Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

// Função recursiva para calcular o fatorial de um número
int fatorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    }
    return n * fatorial(n - 1);
}

// Função para verificar se um número é primo
bool isPrimo(int n) {
    if (n < 2) {
        return false;
    }
    for (int i = 2; i <= n / 2; ++i) {
        if (n % i == 0) {
            return false;
        }
    }
    return true;
}

// Função para encontrar os números primos em um intervalo
std::vector<int> encontrarPrimos(int inicio, int fim) {
    std::vector<int> primos;
    for (int i = inicio; i <= fim; ++i) {
        if (isPrimo(i)) {
            primos.push_back(i);
        }
    }
    return primos;
}

int main() {
    int numero;
    std::cout << "Digite um número: ";
    std::cin >> numero;

    int fatorialNumero = fatorial(numero);
    std::cout << "O fatorial de " << numero << " é: " << fatorialNumero << std::endl;

    std::cout << "Digite dois números para encontrar os primos no intervalo: ";
    int inicio, fim;
    std::cin >> inicio >> fim;

    std::vector<int> primos = encontrarPrimos(inicio, fim);

    std::cout << "Os números primos entre " << inicio << " e " << fim << " são: ";
    for (int primo : primos) {
        std::cout << primo << " ";
    }
    std::cout << std::endl;

    return 0;
}
```

Neste código, temos três funções. A primeira função `fatorial` é uma função recursiva que calcula o fatorial de um número dado. A segunda função `isPrimo` verifica se um número é primo. A terceira função `encontrarPrimos` encontra todos os números primos em um determinado intervalo.

No `main`, o programa solicitará ao usuário que digite um número. Em seguida, calculará o fatorial desse número usando a função `fatorial` e exibirá o resultado.

Depois, o programa solicitará ao usuário que digite dois números para definir um intervalo. Em seguida, usará a função `encontrarPrimos` para encontrar e armazenar todos os números primos encontrados nesse intervalo em um vetor. Por fim, o programa exibirá todos os números primos encontrados.

Esse código é um exemplo de complexidade e uso de funções, mostrando diferentes conceitos da linguagem C++.