Claro! Aqui está um código complexo em C++ que utiliza estruturas de repetição, condicionais e funções para calcular a sequência de Fibonacci até um número especificado pelo usuário:

```cpp
#include <iostream>

// Função para calcular a sequência de Fibonacci
void calcularFibonacci(int n) {
    int numAnterior = 0;
    int numAtual = 1;
    int proximoNum;

    std::cout << "Sequencia de Fibonacci até o " << n << "o termo:" << std::endl;

    // Imprime os primeiros n termos da sequência de Fibonacci
    for (int i = 1; i <= n; i++) {
        std::cout << numAtual << " ";

        proximoNum = numAnterior + numAtual;
        numAnterior = numAtual;
        numAtual = proximoNum;
    }
}

// Função principal
int main() {
    int numero;

    std::cout << "Digite o numero de termos da sequencia de Fibonacci que deseja calcular: ";
    std::cin >> numero;

    calcularFibonacci(numero);

    return 0;
}
```

Nesse código, a função `calcularFibonacci` recebe um número inteiro `n` como parâmetro e calcula os `n` primeiros termos da sequência de Fibonacci. A função utiliza três variáveis para armazenar o número anterior, o número atual e o próximo número da sequência.

Na função `main`, o usuário é solicitado a digitar o número de termos da sequência de Fibonacci que deseja calcular. O valor digitado é então passado como argumento para a função `calcularFibonacci`, que imprime a sequência de Fibonacci até o termo desejado.