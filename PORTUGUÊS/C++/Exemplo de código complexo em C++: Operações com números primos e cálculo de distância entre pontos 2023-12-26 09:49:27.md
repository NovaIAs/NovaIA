Claro! Aqui está um exemplo de código complexo em C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

// Função auxiliar para calcular o fatorial de um número
int calcularFatorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    } else {
        return n * calcularFatorial(n - 1);
    }
}

// Função para verificar se um número é primo
bool verificarPrimo(int n) {
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

// Classe para representar um ponto no plano cartesiano
class Ponto {
public:
    int x, y;

    Ponto(int _x, int _y) {
        x = _x;
        y = _y;
    }

    // Método para calcular a distância entre dois pontos
    double calcularDistancia(Ponto outroPonto) {
        int deltaX = x - outroPonto.x;
        int deltaY = y - outroPonto.y;
        return sqrt(pow(deltaX, 2) + pow(deltaY, 2));
    }
};

int main() {
    // Vetor para armazenar números primos
    vector<int> primos;

    // Preenche o vetor com os números primos entre 1 e 100
    for (int i = 1; i <= 100; i++) {
        if (verificarPrimo(i)) {
            primos.push_back(i);
        }
    }

    // Imprime os números primos
    cout << "Números primos entre 1 e 100: ";
    for (int primo : primos) {
        cout << primo << " ";
    }
    cout << endl;

    // Calcula e imprime o fatorial dos números primos
    cout << "Fatorial dos números primos: ";
    for (int primo : primos) {
        cout << calcularFatorial(primo) << " ";
    }
    cout << endl;

    // Cria dois pontos e calcula a distância entre eles
    Ponto pontoA(3, 4);
    Ponto pontoB(6, 8);
    double distancia = pontoA.calcularDistancia(pontoB);
    cout << "Distância entre os pontos A e B: " << distancia << endl;

    return 0;
}
```

Este código em C++ realiza diversas operações:

1. Inclui os cabeçalhos necessários (iostream, vector e cmath).
2. Possui uma função auxiliar `calcularFatorial` que calcula o fatorial de um número.
3. Possui uma função `verificarPrimo` que verifica se um número é primo.
4. Define uma classe `Ponto` para representar um ponto no plano cartesiano, com um método `calcularDistancia` para calcular a distância entre dois pontos.
5. No `main`, cria um vetor `primos` para armazenar números primos entre 1 e 100.
6. Preenche o vetor com os números primos.
7. Imprime os números primos.
8. Calcula e imprime o fatorial dos números primos.
9. Cria dois pontos e calcula a distância entre eles.
10. Retorna 0.

Espero que este código atenda às suas expectativas!