Claro! Aqui está um exemplo de um código complexo em C++ que utiliza diversos conceitos, estruturas e bibliotecas:

```cpp
#include <iostream>
#include <cmath>
#include <vector>

using namespace std;

// Função para calcular o fatorial de um número
int fatorial(int n) {
    int result = 1;
    for (int i = 1; i <= n; i++) {
        result *= i;
    }
    return result;
}

// Função para calcular o número binomial usando o fatorial
int binomial(int n, int k) {
    return fatorial(n) / (fatorial(k) * fatorial(n - k));
}

// Classe que representa um ponto em um plano cartesiano
class Ponto {
private:
    double x;
    double y;
    
public:
    Ponto(double x, double y) {
        this->x = x;
        this->y = y;
    }
    
    double calcularDistancia(Ponto outroPonto) {
        double deltaX = outroPonto.x - x;
        double deltaY = outroPonto.y - y;
        return sqrt(deltaX * deltaX + deltaY * deltaY);
    }
};

int main() {
    cout << "*** Programa Complexo ***" << endl;
    
    // Vetor de pontos
    vector<Ponto> pontos;
    
    // Preenche o vetor de pontos com pontos aleatórios
    for (int i = 0; i < 5; i++) {
        double x = rand() % 100;
        double y = rand() % 100;
        pontos.push_back(Ponto(x, y));
    }
    
    // Imprime a distância entre todos os pares de pontos
    for (int i = 0; i < pontos.size(); i++) {
        for (int j = i + 1; j < pontos.size(); j++) {
            double distancia = pontos[i].calcularDistancia(pontos[j]);
            cout << "Distância entre o ponto " << i << " e o ponto " << j << ": " << distancia << endl;
        }
    }
    
    // Calcula e imprime alguns números binomiais
    cout << "Números binomiais: " << endl;
    for (int n = 0; n <= 5; n++) {
        for (int k = 0; k <= n; k++) {
            int coeficiente = binomial(n, k);
            cout << "C(" << n << "," << k << ") = " << coeficiente << endl;
        }
    }
    
    return 0;
}
```

Neste código, temos a definição de duas funções: `fatorial` e `binomial`. A função `fatorial` calcula o fatorial de um número usando um loop for, enquanto a função `binomial` calcula o coeficiente binomial usando a função `fatorial`.

Além disso, temos a definição da classe `Ponto`, que representa um ponto em um plano cartesiano. A classe possui um construtor para inicializar as coordenadas x e y, e um método `calcularDistancia` para calcular a distância entre dois pontos.

No `main`, temos um vetor de pontos que é preenchido com pontos aleatórios. Em seguida, é calculada e impressa a distância entre todos os pares de pontos. Também são calculados e impressos alguns números binomiais.

Espero que este código complexo em C++ tenha atendido às suas expectativas! Lembre-se de que, como sou uma inteligência artificial, posso gerar códigos ainda mais complexos e personalizados de acordo com suas necessidades.