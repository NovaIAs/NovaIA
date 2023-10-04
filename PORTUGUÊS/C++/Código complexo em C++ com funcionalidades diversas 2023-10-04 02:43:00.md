Claro! Aqui está um código complexo em C++ que utiliza diversas funcionalidades da linguagem:

```cpp
#include <iostream>
#include <cmath>

// Função recursiva para calcular o fatorial de um número
int fatorial(int n) {
    if (n == 0) {
        return 1;
    }
    return n * fatorial(n - 1);
}

// Função que verifica se um número é primo
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

// Classe abstrata Figura
class Figura {
public:
    virtual float calcularArea() = 0;
    virtual float calcularPerimetro() = 0;
};

// Classe Círculo que herda de Figura
class Circulo : public Figura {
private:
    float raio;
public:
    Circulo(float r) : raio(r) {}

    float calcularArea() override {
        return 3.14159 * pow(raio, 2);
    }

    float calcularPerimetro() override {
        return 2 * 3.14159 * raio;
    }
};

// Classe Quadrado que herda de Figura
class Quadrado : public Figura {
private:
    float lado;
public:
    Quadrado(float l) : lado(l) {}

    float calcularArea() override {
        return pow(lado, 2);
    }

    float calcularPerimetro() override {
        return 4 * lado;
    }
};

int main() {
    int numero;
    std::cout << "Digite um número inteiro: ";
    std::cin >> numero;

    std::cout << "O fatorial de " << numero << " é: " << fatorial(numero) << std::endl;

    if (ehPrimo(numero)) {
        std::cout << numero << " é um número primo" << std::endl;
    } else {
        std::cout << numero << " não é um número primo" << std::endl;
    }

    float raio;
    std::cout << "Digite o raio de um círculo: ";
    std::cin >> raio;

    Circulo circulo(raio);
    std::cout << "A área do círculo é: " << circulo.calcularArea() << std::endl;
    std::cout << "O perímetro do círculo é: " << circulo.calcularPerimetro() << std::endl;

    float lado;
    std::cout << "Digite o lado de um quadrado: ";
    std::cin >> lado;

    Quadrado quadrado(lado);
    std::cout << "A área do quadrado é: " << quadrado.calcularArea() << std::endl;
    std::cout << "O perímetro do quadrado é: " << quadrado.calcularPerimetro() << std::endl;

    return 0;
}
```

Neste código, temos diversas funcionalidades:
1. A função `fatorial` que calcula o fatorial de um número de forma recursiva.
2. A função `ehPrimo` que verifica se um número é primo.
3. A classe abstrata `Figura` que possui métodos virtuais puros para calcular a área e o perímetro de qualquer figura.
4. As classes `Circulo` e `Quadrado` que herdam de `Figura` e implementam seus métodos.
5. No `main`, são solicitados ao usuário um número inteiro, o raio de um círculo e o lado de um quadrado. Em seguida, são feitos cálculos utilizando as funções e as classes criadas, e os resultados são exibidos na tela.

Espero que este código atenda às suas expectativas!