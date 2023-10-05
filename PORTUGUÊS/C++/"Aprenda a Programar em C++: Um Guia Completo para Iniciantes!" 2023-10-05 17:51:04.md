Claro! Aqui está um exemplo de um código complexo em C++ que utiliza múltiplas funcionalidades da linguagem:

```cpp
#include <iostream>
#include <vector>
#include <cmath>

// Função para verificar se um número é primo
bool isPrime(int num) {
    if (num <= 1) {
        return false;
    }
    for (int i = 2; i <= sqrt(num); i++) {
        if (num % i == 0) {
            return false;
        }
    }
    return true;
}

// Função para calcular o fatorial de um número
int factorial(int num) {
    int result = 1;
    for (int i = 2; i <= num; i++) {
        result *= i;
    }
    return result;
}

// Função para imprimir os elementos de um vetor
void printVector(const std::vector<int>& vec) {
    for (int num : vec) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
}

int main() {
    int n;

    std::cout << "Digite um número inteiro positivo: ";
    std::cin >> n;

    std::vector<int> primeNumbers;
    std::vector<int> factorials;

    // Preenche os vetores 'primeNumbers' e 'factorials' com os números primos e fatoriais até o número 'n'
    for (int i = 2; i <= n; i++) {
        if (isPrime(i)) {
            primeNumbers.push_back(i);
        }
        factorials.push_back(factorial(i));
    }

    std::cout << "Números primos até " << n << ": ";
    printVector(primeNumbers);

    std::cout << "Fatoriais até " << n << ": ";
    printVector(factorials);

    return 0;
}
```

Neste código, temos uma função `isPrime` que verifica se um número é primo, utilizando o conceito de divisão por todos os números menores ou iguais à raiz quadrada do número em questão.

Temos também uma função `factorial` que calcula o fatorial de um número, utilizando um laço de repetição para multiplicar todos os números de 2 até o número em questão.

A função `printVector` é responsável por imprimir os elementos de um vetor, recebendo-o como argumento e percorrendo-o com um laço de repetição.

No `main`, o usuário é solicitado a digitar um número inteiro positivo. Em seguida, são criados dois vetores: `primeNumbers` para armazenar os números primos até o número digitado, e `factorials` para armazenar os fatoriais até o número digitado.

Um laço `for` percorre todos os números de 2 até o número digitado, chamando a função `isPrime` para verificar se o número é primo e, caso seja, adiciona-o ao vetor `primeNumbers`. Além disso, a função `factorial` é chamada para calcular o fatorial de cada número e adicioná-lo ao vetor `factorials`.

Por fim, os vetores são impressos na tela utilizando a função `printVector`.

Espero que este exemplo atenda às suas expectativas!