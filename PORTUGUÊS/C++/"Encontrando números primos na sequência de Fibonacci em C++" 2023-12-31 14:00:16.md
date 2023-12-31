Claro! Aqui está um código complexo em C++ que atende aos seus requisitos:

```cpp
#include <iostream>
#include <vector>

// Função para verificar se um número é primo
bool isPrimo(int n) {
    if (n <= 1) {
        return false;
    }

    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            return false;
        }
    }

    return true;
}

// Função para calcular a sequência de Fibonacci até o valor n
std::vector<int> fibonacci(int n) {
    std::vector<int> fib;
    fib.push_back(0);
    fib.push_back(1);

    while (fib.back() < n) {
        int next = fib[fib.size() - 1] + fib[fib.size() - 2];
        fib.push_back(next);
    }

    return fib;
}

int main() {
    // Solicita ao usuário um valor limite para a sequência de Fibonacci
    int limite;
    std::cout << "Digite um valor limite para a sequencia de Fibonacci: ";
    std::cin >> limite;

    // Calcula a sequência de Fibonacci até o valor limite
    std::vector<int> fib = fibonacci(limite);

    // Exibe os números primos da sequência de Fibonacci
    std::cout << "Numeros primos na sequencia de Fibonacci ate " << limite << ":" << std::endl;
    for (int i = 0; i < fib.size(); i++) {
        if (isPrimo(fib[i])) {
            std::cout << fib[i] << " ";
        }
    }

    std::cout << std::endl;

    return 0;
}
```

Este código em C++ solicita ao usuário um valor limite para a sequência de Fibonacci. Em seguida, ele calcula a sequência de Fibonacci até o valor limite fornecido. Por fim, o código exibe os números primos presentes na sequência de Fibonacci.

A função `isPrimo()` é utilizada para verificar se um número é primo. Ela verifica se o número é menor ou igual a 1 (caso seja, retorna falso) e, em seguida, percorre todos os números de 2 até a raiz quadrada do número, verificando se há algum divisor. Se encontrar um divisor, retorna falso; caso contrário, retorna verdadeiro.

A função `fibonacci()` é responsável por calcular a sequência de Fibonacci até o valor fornecido. Ela inicializa um vetor com os primeiros dois números da sequência (0 e 1) e, em seguida, continua adicionando números à sequência até que o último número seja maior que o valor limite fornecido.

No `main()`, o código solicita ao usuário um valor limite, chama a função `fibonacci()` para calcular a sequência até esse limite e, em seguida, percorre a sequência para verificar quais números são primos. Os números primos são exibidos na saída padrão.

Espero que este código atenda às suas expectativas! Lembre-se que é um exemplo complexo e específico, mas pode ser adaptado para outros problemas ou necessidades.