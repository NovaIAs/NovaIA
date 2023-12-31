Claro! Aqui está um código complexo em Python que envolve uma implementação de um algoritmo de otimização por enxame de partículas (Particle Swarm Optimization - PSO) para resolver o problema do caixeiro-viajante:

```python
import numpy as np

class Particle:
    def __init__(self, num_cities):
        self.position = np.random.permutation(num_cities)
        self.velocity = np.zeros(num_cities)
        self.pbest_position = self.position.copy()
        self.pbest_fitness = float('inf')

class PSO:
    def __init__(self, num_particles, num_iterations, num_cities, cities_matrix, c1=2.0, c2=2.0, w=0.9):
        self.num_particles = num_particles
        self.num_iterations = num_iterations
        self.num_cities = num_cities
        self.cities_matrix = cities_matrix
        self.c1 = c1
        self.c2 = c2
        self.w = w
        self.gbest_position = np.zeros(num_cities)
        self.gbest_fitness = float('inf')
        self.particles = []

    def initialize_particles(self):
        for _ in range(self.num_particles):
            particle = Particle(self.num_cities)
            self.particles.append(particle)

    def calculate_fitness(self, particle):
        distance = 0
        for i in range(self.num_cities - 1):
            city_a = particle.position[i]
            city_b = particle.position[i+1]
            distance += self.cities_matrix[city_a][city_b]
        distance += self.cities_matrix[particle.position[-1]][particle.position[0]]
        return distance

    def update_particle(self, particle):
        for i in range(self.num_cities):
            r1 = np.random.random()
            r2 = np.random.random()

            particle.velocity[i] = (self.w * particle.velocity[i] +
                                    self.c1 * r1 * (particle.pbest_position[i] - particle.position[i]) +
                                    self.c2 * r2 * (self.gbest_position[i] - particle.position[i]))

            particle.position[i] += particle.velocity[i]

            if particle.position[i] < 0:
                particle.position[i] = 0
            elif particle.position[i] >= self.num_cities:
                particle.position[i] = self.num_cities - 1

    def update_gbest(self):
        for particle in self.particles:
            fitness = self.calculate_fitness(particle)
            if fitness < self.gbest_fitness:
                self.gbest_fitness = fitness
                self.gbest_position = particle.position.copy()

    def run(self):
        self.initialize_particles()
        for _ in range(self.num_iterations):
            for particle in self.particles:
                fitness = self.calculate_fitness(particle)
                if fitness < particle.pbest_fitness:
                    particle.pbest_fitness = fitness
                    particle.pbest_position = particle.position.copy()
                self.update_particle(particle)
            self.update_gbest()

        return self.gbest_position, self.gbest_fitness

# Exemplo de uso
cities_matrix = np.array([[0, 10, 15, 20],
                          [10, 0, 35, 25],
                          [15, 35, 0, 30],
                          [20, 25, 30, 0]])

num_cities = cities_matrix.shape[0]
num_particles = 50
num_iterations = 100

pso = PSO(num_particles, num_iterations, num_cities, cities_matrix)
best_path, best_distance = pso.run()

print("Melhor caminho encontrado:", best_path)
print("Distância percorrida:", best_distance)
```

Neste código, a classe `Particle` representa uma partícula no algoritmo PSO e a classe `PSO` é responsável pela implementação do algoritmo PSO em si. 

A função `initialize_particles` é utilizada para inicializar as partículas do enxame, onde cada partícula é representada por uma permutação aleatória das cidades. A função `calculate_fitness` calcula a distância total percorrida por uma partícula. A função `update_particle` é responsável por atualizar a velocidade e posição de uma partícula, levando em consideração a posição atual, a melhor posição pessoal (`pbest`) da partícula e a melhor posição global (`gbest`) de todo o enxame. A função `update_gbest` é responsável por atualizar a melhor posição global encontrada até o momento. Por fim, o método `run` executa o algoritmo PSO por um determinado número de iterações.

O exemplo de uso no final do código ilustra como utilizar a classe `PSO` para resolver o problema do caixeiro-viajante com uma matriz de distâncias predefinida. Neste exemplo, o código encontra o melhor caminho e a distância percorrida pelo caixeiro-viajante, utilizando 50 partículas e 100 iterações.