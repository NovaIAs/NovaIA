Claro! Aqui está um exemplo de um código complexo em C++ que realiza o cálculo do fatorial de um número, utilizando recursão:

```cpp
#include <iostream>

long long int calcularFatorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    } else {
        return n * calcularFatorial(n - 1);
    }
}

int main() {
    int numero;

    std::cout << "Digite um número inteiro não negativo: ";
    std::cin >> numero;

    if (numero < 0) {
        std::cout << "Erro! O número deve ser não negativo." << std::endl;
        return 1;
    }

    long long int resultado = calcularFatorial(numero);

    std::cout << "O fatorial de " << numero << " é: " << resultado << std::endl;

    return 0;
}
```

Neste código, a função `calcularFatorial` é responsável por calcular o fatorial de um número. Se o número for 0 ou 1, o valor retornado é 1. Caso contrário, a função chama a si mesma passando `n - 1` como argumento e multiplica o resultado por `n`. No final, a função `main` solicita ao usuário que digite um número inteiro não negativo, verifica se o número é válido e, em seguida, chama a função `calcularFatorial` com o número digitado. O resultado é exibido na tela.