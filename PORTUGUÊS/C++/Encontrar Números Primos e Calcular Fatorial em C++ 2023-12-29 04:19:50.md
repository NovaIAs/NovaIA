Claro! Aqui está um exemplo de um código complexo em C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>

// Função para verificar se um número é primo
bool ehPrimo(int n) {
    if (n <= 1) {
        return false;
    }
    
    for (int i = 2; i <= sqrt(n); i++) {
        if (n % i == 0) {
            return false;
        }
    }
    
    return true;
}

// Função para encontrar todos os números primos em um intervalo
std::vector<int> encontrarPrimos(int inicio, int fim) {
    std::vector<int> primos;
    
    for (int i = inicio; i <= fim; i++) {
        if (ehPrimo(i)) {
            primos.push_back(i);
        }
    }
    
    return primos;
}

// Função para calcular o fatorial de um número
int fatorial(int n) {
    if (n <= 1) {
        return 1;
    }
    
    return n * fatorial(n - 1);
}

// Função principal
int main() {
    int limiteInferior, limiteSuperior;
    
    std::cout << "Digite o limite inferior: ";
    std::cin >> limiteInferior;
    
    std::cout << "Digite o limite superior: ";
    std::cin >> limiteSuperior;
    
    std::vector<int> numerosPrimos = encontrarPrimos(limiteInferior, limiteSuperior);
    
    std::cout << "Números primos encontrados: ";
    for (int i = 0; i < numerosPrimos.size(); i++) {
        std::cout << numerosPrimos[i] << " ";
    }
    std::cout << std::endl;
    
    std::cout << "Digite um número para calcular o seu fatorial: ";
    int numero;
    std::cin >> numero;
    
    int resultadoFatorial = fatorial(numero);
    
    std::cout << "O fatorial de " << numero << " é: " << resultadoFatorial << std::endl;
    
    return 0;
}
```

Neste código, temos algumas funções úteis. A função `ehPrimo` verifica se um número é primo, percorrendo os números de 2 até a raiz quadrada do número e verificando se há algum divisor. A função `encontrarPrimos` utiliza a função `ehPrimo` para encontrar todos os números primos em um intervalo dado.

Além disso, temos a função `fatorial` que calcula o fatorial de um número utilizando recursão.

Na função principal, o programa solicita ao usuário um limite inferior e um limite superior, e então encontra todos os números primos nesse intervalo utilizando a função `encontrarPrimos`. Em seguida, o programa solicita ao usuário um número para calcular o seu fatorial utilizando a função `fatorial`.

Por fim, o programa exibe os números primos encontrados e o resultado do fatorial.