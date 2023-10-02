Claro! Aqui está um exemplo de código complexo em C++:

```cpp
#include <iostream>
#include <cmath>

// Função para verificar se um número é primo
bool ehPrimo(int n)
{
    // Se o número for menor ou igual a 1, não é primo
    if (n <= 1)
        return false;

    // Verifica se o número é divisível por algum número até sua raiz quadrada
    for (int i = 2; i <= std::sqrt(n); i++)
    {
        if (n % i == 0)
            return false;
    }

    // Se não for divisível por nenhum número, é primo
    return true;
}

// Função para calcular o fatorial de um número
int fatorial(int n)
{
    // Caso base: fatorial de 0 é 1
    if (n == 0)
        return 1;

    // Calcula o fatorial recursivamente
    return n * fatorial(n - 1);
}

int main()
{
    // Solicita um número ao usuário
    int numero;
    std::cout << "Digite um numero: ";
    std::cin >> numero;

    // Verifica se o número é primo
    if (ehPrimo(numero))
    {
        std::cout << "O numero " << numero << " eh primo." << std::endl;
    }
    else
    {
        std::cout << "O numero " << numero << " nao eh primo." << std::endl;
    }

    // Calcula o fatorial do número
    int fat = fatorial(numero);
    std::cout << "O fatorial de " << numero << " eh: " << fat << std::endl;

    return 0;
}
```

Neste código, temos duas funções: `ehPrimo` e `fatorial`. A função `ehPrimo` verifica se um número é primo, enquanto a função `fatorial` calcula o fatorial de um número.

O programa principal solicita ao usuário um número e, em seguida, verifica se esse número é primo. Se for primo, exibe uma mensagem indicando isso; caso contrário, exibe uma mensagem informando que não é primo.

Em seguida, o programa calcula o fatorial do número digitado pelo usuário e exibe o resultado.

Espero que este código complexo em C++ atenda às suas expectativas!