Claro! Aqui está um exemplo de código complexo em C++:

```cpp
#include <iostream>
#include <vector>

// Função para calcular o fatorial de um número
int fatorial(int n) {
    if (n <= 1) {
        return 1;
    }
    else {
        return n * fatorial(n - 1);
    }
}

// Classe para representar um ponto no espaço tridimensional
class Ponto {
private:
    double x, y, z;

public:
    Ponto(double x, double y, double z) {
        this->x = x;
        this->y = y;
        this->z = z;
    }

    double calcularDistancia(const Ponto& p) {
        double dx = this->x - p.x;
        double dy = this->y - p.y;
        double dz = this->z - p.z;
        return sqrt(dx * dx + dy * dy + dz * dz);
    }
};

int main() {
    // Vetor de números
    std::vector<int> numeros = { 1, 2, 3, 4, 5 };

    // Imprime os números
    std::cout << "Números: ";
    for (int num : numeros) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    // Calcula o fatorial de 5
    int fatorialDe5 = fatorial(5);
    std::cout << "Fatorial de 5: " << fatorialDe5 << std::endl;

    // Cria dois pontos no espaço tridimensional
    Ponto p1(0.0, 0.0, 0.0);
    Ponto p2(1.0, 1.0, 1.0);

    // Calcula a distância entre os dois pontos
    double distancia = p1.calcularDistancia(p2);
    std::cout << "Distância entre os pontos: " << distancia << std::endl;

    return 0;
}
```

Explicação do código:

1. Incluímos as bibliotecas `<iostream>` e `<vector>` para usar as funcionalidades de entrada/saída e vetores.
2. Definimos a função `fatorial` que calcula o fatorial de um número recursivamente.
3. Definimos a classe `Ponto` que representa um ponto no espaço tridimensional. Ela possui três coordenadas: `x`, `y` e `z`.
4. No `main()`, criamos um vetor de números e o imprimimos na tela.
5. Calculamos o fatorial do número 5 e o imprimimos na tela.
6. Criamos dois pontos no espaço tridimensional (`p1` e `p2`) e calculamos a distância entre eles usando o método `calcularDistancia()` da classe `Ponto`.
7. Imprimimos a distância na tela.
8. Retornamos 0 para indicar que o programa foi executado com sucesso.