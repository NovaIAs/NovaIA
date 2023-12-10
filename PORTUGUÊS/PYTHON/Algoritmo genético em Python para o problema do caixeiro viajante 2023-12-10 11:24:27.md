Claro! Vou gerar um código em Python que implementa um algoritmo genético para resolver o problema do caixeiro viajante. O caixeiro viajante é um problema de otimização combinatória, onde o objetivo é encontrar a menor rota possível que passe por todas as cidades, retornando à cidade de origem. 

Aqui está o código:

```python
import random

# Definição do problema do caixeiro viajante
class ProblemaCaixeiroViajante:
    def __init__(self, cidades, distancia):
        self.cidades = cidades
        self.distancia = distancia

    def calcular_distancia(self, rota):
        distancia_total = 0
        for i in range(len(rota) - 1):
            cidade_atual = rota[i]
            proxima_cidade = rota[i + 1]
            distancia_total += self.distancia[cidade_atual][proxima_cidade]
        distancia_total += self.distancia[rota[-1]][rota[0]]  # retorna à cidade de origem
        return distancia_total

# Algoritmo genético para resolver o problema do caixeiro viajante
class AlgoritmoGenetico:
    def __init__(self, problema, tamanho_populacao, taxa_mutacao):
        self.problema = problema
        self.tamanho_populacao = tamanho_populacao
        self.taxa_mutacao = taxa_mutacao

    def gerar_populacao_inicial(self):
        populacao = []
        for _ in range(self.tamanho_populacao):
            rota = random.sample(self.problema.cidades, len(self.problema.cidades))
            populacao.append(rota)
        return populacao

    def selecionar_pais(self, populacao):
        pais = random.sample(populacao, 2)
        return pais

    def crossover(self, pai1, pai2):
        ponto_corte = random.randint(1, len(pai1) - 2)
        filho1 = pai1[:ponto_corte] + [cidade for cidade in pai2 if cidade not in pai1[:ponto_corte]]
        filho2 = pai2[:ponto_corte] + [cidade for cidade in pai1 if cidade not in pai2[:ponto_corte]]
        return filho1, filho2

    def mutacao(self, filho):
        for i in range(len(filho)):
            if random.random() < self.taxa_mutacao:
                j = random.randint(0, len(filho) - 1)
                filho[i], filho[j] = filho[j], filho[i]
        return filho

    def resolver(self, numero_geracoes):
        populacao = self.gerar_populacao_inicial()
        melhor_distancia = float('inf')
        melhor_rota = None

        for _ in range(numero_geracoes):
            nova_populacao = []
            for _ in range(self.tamanho_populacao // 2):
                pai1, pai2 = self.selecionar_pais(populacao)
                filho1, filho2 = self.crossover(pai1, pai2)
                filho1 = self.mutacao(filho1)
                filho2 = self.mutacao(filho2)
                nova_populacao.extend([filho1, filho2])
            
            populacao = nova_populacao

            for individuo in populacao:
                distancia = self.problema.calcular_distancia(individuo)
                if distancia < melhor_distancia:
                    melhor_distancia = distancia
                    melhor_rota = individuo
        
        return melhor_rota, melhor_distancia

# Exemplo de uso do algoritmo genético
cidades = ["A", "B", "C", "D", "E"]
distancia = {
    "A": {"A": 0, "B": 10, "C": 15, "D": 20, "E": 25},
    "B": {"A": 10, "B": 0, "C": 35, "D": 25, "E": 20},
    "C": {"A": 15, "B": 35, "C": 0, "D": 30, "E": 45},
    "D": {"A": 20, "B": 25, "C": 30, "D": 0, "E": 50},
    "E": {"A": 25, "B": 20, "C": 45, "D": 50, "E": 0}
}

problema = ProblemaCaixeiroViajante(cidades, distancia)
algoritmo_genetico = AlgoritmoGenetico(problema, tamanho_populacao=100, taxa_mutacao=0.1)
melhor_rota, melhor_distancia = algoritmo_genetico.resolver(numero_geracoes=1000)

print("Melhor rota encontrada:", melhor_rota)
print("Melhor distância encontrada:", melhor_distancia)
```

Explicação do código:

1. Começamos definindo a classe `ProblemaCaixeiroViajante`, que representa o problema em si. Ela recebe uma lista de cidades e uma matriz de distâncias entre as cidades.

2. Em seguida, definimos a classe `AlgoritmoGenetico`, que implementa o algoritmo genético para resolver o problema do caixeiro viajante. Ela recebe o problema, o tamanho da população e a taxa de mutação como parâmetros.

3. O método `gerar_populacao_inicial` gera uma população inicial aleatória de rotas, onde cada rota é uma permutação das cidades.

4. O método `selecionar_pais` seleciona dois indivíduos aleatórios da população como pais para gerar a próxima geração.

5. O método `crossover` realiza o crossover entre dois pais, criando dois filhos. O ponto de corte é escolhido aleatoriamente.

6. O método `mutacao` realiza a mutação em um filho, trocando duas cidades de posição com uma determinada probabilidade.

7. O método `resolver` é responsável por executar o algoritmo genético. Ele itera sobre um número fixo de gerações, gerando uma nova população a cada iteração.

8. Por fim, temos um exemplo de uso do algoritmo genético para resolver o problema do caixeiro viajante com um conjunto de cidades e distâncias pré-definidas. A melhor rota encontrada e a melhor distância são impressas na saída.

Espero que este código atenda às suas expectativas!