Claro! Aqui está um exemplo de um código complexo em C++ que envolve uma série de conceitos avançados:

```cpp
#include <iostream>
#include <cmath>
#include <vector>

// Função recursiva para calcular o fatorial de um número
int fatorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    }
    return n * fatorial(n - 1);
}

// Classe para representar um ponto no plano cartesiano
class Ponto {
private:
    double x, y;

public:
    Ponto(double x_, double y_) : x(x_), y(y_) {}

    double distancia(Ponto p) {
        return sqrt(pow(p.x - x, 2) + pow(p.y - y, 2));
    }
};

int main() {
    // Vetor de pontos
    std::vector<Ponto> pontos;

    // Adicionando pontos ao vetor
    pontos.push_back(Ponto(0, 0));
    pontos.push_back(Ponto(1, 1));
    pontos.push_back(Ponto(2, 3));

    // Calculando a distância entre os pontos
    double distanciaTotal = 0;
    for (int i = 0; i < pontos.size() - 1; i++) {
        distanciaTotal += pontos[i].distancia(pontos[i + 1]);
    }

    std::cout << "Distância total percorrida: " << distanciaTotal << std::endl;

    // Calculando o fatorial de um número
    int num = 5;
    int resultado = fatorial(num);
    std::cout << "O fatorial de " << num << " é " << resultado << std::endl;

    return 0;
}
```

Explicação:

Nesse código, temos a definição de uma função chamada `fatorial`, que é implementada de forma recursiva para calcular o fatorial de um número inteiro. A função verifica se o número é igual a 0 ou 1 e retorna 1 nesses casos, caso contrário, chama a função novamente com o número decrementado e multiplica pelo próprio número.

Além disso, temos uma classe chamada `Ponto` que representa um ponto no plano cartesiano. Essa classe possui dois atributos privados, `x` e `y`, e um método público chamado `distancia` que calcula a distância entre dois pontos utilizando a fórmula da distância euclidiana.

No `main`, criamos um vetor de pontos e adicionamos três objetos da classe `Ponto` a ele. Em seguida, calculamos a distância total percorrida percorrendo o vetor de pontos e acumulando a distância entre cada ponto e o próximo. Imprimimos o resultado na tela.

Ainda no `main`, calculamos o fatorial de um número (5 nesse caso) utilizando a função `fatorial` e imprimimos o resultado na tela.

Esse código demonstra o uso de recursão, classes, vetores e funções matemáticas em C++. É um exemplo mais complexo que envolve múltiplos conceitos e pode ser útil para fins de estudo e prática.