Claro! Vou gerar um código complexo em C++ para você. Este código é um exemplo de um algoritmo de otimização por enxame de partículas, chamado PSO (Particle Swarm Optimization). O algoritmo é usado para encontrar o mínimo global de uma função em um espaço de busca multidimensional.

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <random>

// Definindo o número de partículas
const int NUM_PARTICLES = 100;

// Definindo o número de dimensões do espaço de busca
const int NUM_DIMENSIONS = 2;

// Definindo o limite superior e inferior do espaço de busca
const double UPPER_BOUND = 10.0;
const double LOWER_BOUND = -10.0;

// Definindo o número máximo de iterações
const int MAX_ITERATIONS = 100;

// Definindo o peso da inércia
const double INERTIA_WEIGHT = 0.7;

// Definindo o peso cognitivo
const double COGNITIVE_WEIGHT = 1.4;

// Definindo o peso social
const double SOCIAL_WEIGHT = 1.4;

// Função objetivo para minimizar (exemplo: função de Rosenbrock)
double objectiveFunction(const std::vector<double>& position) {
    double sum = 0.0;
    for (int i = 0; i < NUM_DIMENSIONS - 1; i++) {
        double x = position[i];
        double y = position[i + 1];
        double term1 = std::pow(y - x * x, 2);
        double term2 = std::pow(1 - x, 2);
        sum += 100 * term1 + term2;
    }
    return sum;
}

// Classe para representar uma partícula
class Particle {
public:
    std::vector<double> position;
    std::vector<double> velocity;
    std::vector<double> bestPosition;
    double bestFitness;

    Particle() {
        position.resize(NUM_DIMENSIONS);
        velocity.resize(NUM_DIMENSIONS);
        bestPosition.resize(NUM_DIMENSIONS);
        bestFitness = std::numeric_limits<double>::max();
    }

    void initialize() {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<double> dis(LOWER_BOUND, UPPER_BOUND);
        for (int i = 0; i < NUM_DIMENSIONS; i++) {
            position[i] = dis(gen);
            velocity[i] = dis(gen);
        }
        bestPosition = position;
        bestFitness = objectiveFunction(position);
    }

    void update(const std::vector<double>& globalBestPosition) {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<double> dis(0.0, 1.0);
        for (int i = 0; i < NUM_DIMENSIONS; i++) {
            double r1 = dis(gen);
            double r2 = dis(gen);
            velocity[i] = INERTIA_WEIGHT * velocity[i] +
                          COGNITIVE_WEIGHT * r1 * (bestPosition[i] - position[i]) +
                          SOCIAL_WEIGHT * r2 * (globalBestPosition[i] - position[i]);
            position[i] += velocity[i];
            position[i] = std::max(position[i], LOWER_BOUND);
            position[i] = std::min(position[i], UPPER_BOUND);
        }
        double fitness = objectiveFunction(position);
        if (fitness < bestFitness) {
            bestPosition = position;
            bestFitness = fitness;
        }
    }
};

int main() {
    std::vector<Particle> particles(NUM_PARTICLES);
    std::vector<double> globalBestPosition(NUM_DIMENSIONS);
    double globalBestFitness = std::numeric_limits<double>::max();

    // Inicializando as partículas
    for (int i = 0; i < NUM_PARTICLES; i++) {
        particles[i].initialize();
        if (particles[i].bestFitness < globalBestFitness) {
            globalBestPosition = particles[i].bestPosition;
            globalBestFitness = particles[i].bestFitness;
        }
    }

    // Executando o algoritmo PSO
    int iteration = 0;
    while (iteration < MAX_ITERATIONS) {
        for (int i = 0; i < NUM_PARTICLES; i++) {
            particles[i].update(globalBestPosition);
            if (particles[i].bestFitness < globalBestFitness) {
                globalBestPosition = particles[i].bestPosition;
                globalBestFitness = particles[i].bestFitness;
            }
        }
        iteration++;
    }

    // Resultados
    std::cout << "Melhor posição encontrada: ";
    for (int i = 0; i < NUM_DIMENSIONS; i++) {
        std::cout << globalBestPosition[i] << " ";
    }
    std::cout << std::endl;
    std::cout << "Melhor fitness encontrado: " << globalBestFitness << std::endl;

    return 0;
}
```

Este código implementa um algoritmo PSO para encontrar o mínimo global da função de Rosenbrock, uma função de otimização conhecida. O código cria um conjunto de partículas que se movem em um espaço de busca multidimensional. Cada partícula atualiza sua posição com base em sua velocidade e nas melhores posições encontradas até o momento. O algoritmo continua iterando até atingir o número máximo de iterações.

Espero que isso atenda às suas expectativas!