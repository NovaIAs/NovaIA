Claro! Aqui está um código em Python complexo, que faz uso de conceitos avançados e é explicado em detalhes. O código abaixo é um exemplo de um algoritmo genético para resolver o problema do caixeiro viajante.

```python
import random

class Cidade:
    def __init__(self, nome, x, y):
        self.nome = nome
        self.x = x
        self.y = y

class Populacao:
    def __init__(self, tamanho, cidades):
        self.tamanho = tamanho
        self.cidades = cidades
        self.individuos = []

        for _ in range(tamanho):
            random.shuffle(cidades)
            self.individuos.append(list(cidades))

    def calcular_fitness(self):
        for individuo in self.individuos:
            distancia_total = 0

            for i in range(len(individuo) - 1):
                cidade_atual = individuo[i]
                proxima_cidade = individuo[i + 1]
                distancia = calcular_distancia(cidade_atual, proxima_cidade)
                distancia_total += distancia

            individuo.append(distancia_total)

        self.individuos = sorted(self.individuos, key=lambda x: x[-1])

    def selecionar_pais(self):
        soma_fitness = sum([individuo[-1] for individuo in self.individuos])
        proporcoes = [individuo[-1] / soma_fitness for individuo in self.individuos]

        pais = []

        for _ in range(self.tamanho):
            pai = random.choices(self.individuos, weights=proporcoes)[0]
            pais.append(pai)

        return pais

    def cruzar_pais(self, pais):
        filhos = []

        for i in range(0, len(pais), 2):
            pai1 = pais[i]
            pai2 = pais[i + 1]

            ponto_corte = random.randint(1, len(pai1) - 1)
            filho1 = pai1[:ponto_corte] + [cidade for cidade in pai2 if cidade not in pai1[:ponto_corte]]
            filho2 = pai2[:ponto_corte] + [cidade for cidade in pai1 if cidade not in pai2[:ponto_corte]]

            filhos.append(filho1)
            filhos.append(filho2)

        return filhos

    def mutar_filhos(self, filhos, taxa_mutacao):
        for filho in filhos:
            if random.random() < taxa_mutacao:
                indices = random.sample(range(len(filho)), 2)
                filho[indices[0]], filho[indices[1]] = filho[indices[1]], filho[indices[0]]

    def evoluir(self, geracoes, taxa_mutacao):
        for _ in range(geracoes):
            self.calcular_fitness()
            pais = self.selecionar_pais()
            filhos = self.cruzar_pais(pais)
            self.mutar_filhos(filhos, taxa_mutacao)

            self.individuos = filhos

    def melhor_individuo(self):
        self.calcular_fitness()
        return self.individuos[0]

def calcular_distancia(cidade1, cidade2):
    return ((cidade1.x - cidade2.x) ** 2 + (cidade1.y - cidade2.y) ** 2) ** 0.5

def main():
    cidades = [
        Cidade("A", 0, 0),
        Cidade("B", 1, 3),
        Cidade("C", 2, 1),
        Cidade("D", 4, 2),
        Cidade("E", 5, 0)
    ]

    populacao = Populacao(tamanho=100, cidades=cidades)
    populacao.evoluir(geracoes=1000, taxa_mutacao=0.01)

    melhor_rota = populacao.melhor_individuo()
    melhor_rota_nomes = [cidade.nome for cidade in melhor_rota[:-1]]

    print(f"A melhor rota encontrada foi: {melhor_rota_nomes}")
    print(f"Distância total percorrida: {melhor_rota[-1]}")

if __name__ == "__main__":
    main()
```

Neste código, é implementado um algoritmo genético para resolver o problema do caixeiro viajante. O objetivo é encontrar a melhor rota para visitar um conjunto de cidades, minimizando a distância percorrida.

O código começa definindo uma classe `Cidade`, que representa cada uma das cidades a serem visitadas. Cada cidade possui um nome, coordenada X e coordenada Y.

Em seguida, é definida a classe `Populacao`, que representa a população de indivíduos no algoritmo genético. Cada indivíduo corresponde a uma possível rota a ser percorrida. A população é inicializada com um tamanho especificado e uma lista de cidades. Os indivíduos são criados aleatoriamente a partir dessa lista de cidades.

A população possui métodos para calcular o fitness de cada indivíduo (distância total percorrida), selecionar os pais para reprodução, realizar o cruzamento entre os pais para gerar filhos, realizar mutações nos filhos e evoluir a população por um número de gerações especificado.

A função `calcular_distancia` é utilizada para calcular a distância entre duas cidades, utilizando a fórmula da distância euclidiana.

A função `main` é responsável por criar um conjunto de cidades, inicializar a população, evoluir a população por um número de gerações e imprimir a melhor rota encontrada e a distância total percorrida.

Este código utiliza conceitos avançados de programação, como classes, algoritmos genéticos, ordenação e aleatoriedade. Ele resolve um problema complexo e pode ser adaptado para lidar com um número maior de cidades.