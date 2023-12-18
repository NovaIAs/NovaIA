Certamente! Aqui está um código complexo em Swift que envolve uma implementação de um algoritmo genético para resolver o problema do caixeiro viajante. O código está em português e inclui comentários explicando cada etapa do algoritmo:

```swift
import Foundation

// Definindo a estrutura do problema do caixeiro viajante
struct Cidade {
    let nome: String
    let coordenadas: (x: Int, y: Int)
}

// Definindo o algoritmo genético
struct AlgoritmoGenetico {
    let cidades: [Cidade]
    let tamanhoPopulacao: Int
    let taxaMutacao: Double
    let numeroGeracoes: Int

    // Função para calcular a distância entre duas cidades
    func calcularDistancia(cidadeA: Cidade, cidadeB: Cidade) -> Double {
        let deltaX = Double(cidadeA.coordenadas.x - cidadeB.coordenadas.x)
        let deltaY = Double(cidadeA.coordenadas.y - cidadeB.coordenadas.y)

        return sqrt(pow(deltaX, 2) + pow(deltaY, 2))
    }

    // Função para gerar uma população inicial aleatória
    func gerarPopulacaoInicial() -> [[Cidade]] {
        var populacaoInicial: [[Cidade]] = []

        for _ in 0..<tamanhoPopulacao {
            let populacao = cidades.shuffled()
            populacaoInicial.append(populacao)
        }

        return populacaoInicial
    }

    // Função para calcular o fitness de um indivíduo
    func calcularFitness(individuo: [Cidade]) -> Double {
        var distanciaTotal: Double = 0

        for i in 0..<individuo.count {
            let cidadeAtual = individuo[i]
            let cidadeProxima = individuo[(i + 1) % individuo.count] // Circular

            distanciaTotal += calcularDistancia(cidadeA: cidadeAtual, cidadeB: cidadeProxima)
        }

        return 1 / distanciaTotal
    }

    // Função para selecionar os pais para reprodução usando o método da roleta viciada
    func selecionarPais(populacao: [[Cidade]]) -> [Cidade] {
        let somaFitness = populacao.reduce(0) { $0 + calcularFitness(individuo: $1) }
        let r = Double.random(in: 0..<somaFitness)
        var somaParcial: Double = 0

        for individuo in populacao {
            somaParcial += calcularFitness(individuo: individuo)

            if somaParcial >= r {
                return individuo
            }
        }

        return populacao.last!
    }

    // Função para realizar a reprodução (crossover) entre dois pais
    func reproduzir(paiA: [Cidade], paiB: [Cidade]) -> [Cidade] {
        let pontoCorte = Int.random(in: 0..<paiA.count)
        let parteA = Array(paiA.prefix(upTo: pontoCorte))
        let parteB = paiB.filter { !parteA.contains($0) }

        return parteA + parteB
    }

    // Função para aplicar o operador de mutação em um indivíduo
    func mutar(individuo: [Cidade]) -> [Cidade] {
        var individuoMutado = individuo

        for i in 0..<individuoMutado.count {
            if Double.random(in: 0..<1) < taxaMutacao {
                let j = Int.random(in: 0..<individuoMutado.count)
                individuoMutado.swapAt(i, j)
            }
        }

        return individuoMutado
    }

    // Função para executar o algoritmo genético
    func executar() -> [Cidade] {
        var populacao = gerarPopulacaoInicial()

        for _ in 0..<numeroGeracoes {
            var novaPopulacao: [[Cidade]] = []

            for _ in 0..<tamanhoPopulacao {
                let paiA = selecionarPais(populacao: populacao)
                let paiB = selecionarPais(populacao: populacao)
                var filho = reproduzir(paiA: paiA, paiB: paiB)
                filho = mutar(individuo: filho)

                novaPopulacao.append(filho)
            }

            populacao = novaPopulacao
        }

        let melhorIndividuo = populacao.max { calcularFitness(individuo: $0) < calcularFitness(individuo: $1) }!

        return melhorIndividuo
    }
}

// Exemplo de utilização do algoritmo genético para resolver o problema do caixeiro viajante
let cidades = [
    Cidade(nome: "A", coordenadas: (x: 1, y: 1)),
    Cidade(nome: "B", coordenadas: (x: 2, y: 3)),
    Cidade(nome: "C", coordenadas: (x: 5, y: 2)),
    Cidade(nome: "D", coordenadas: (x: 7, y: 4)),
    Cidade(nome: "E", coordenadas: (x: 6, y: 6))
]

let algoritmoGenetico = AlgoritmoGenetico(cidades: cidades, tamanhoPopulacao: 100, taxaMutacao: 0.01, numeroGeracoes: 1000)
let melhorRota = algoritmoGenetico.executar()

print("Melhor rota encontrada:")
for cidade in melhorRota {
    print(cidade.nome)
}
```

Este código implementa um algoritmo genético para resolver o problema do caixeiro viajante. O algoritmo começa gerando uma população inicial aleatória de rotas entre as cidades fornecidas. Em seguida, ele avalia o fitness de cada indivíduo (rota) calculando a distância total percorrida. O fitness é definido como o inverso da distância total para que maiores valores de fitness correspondam a melhores rotas.

A seleção dos pais para reprodução é feita usando o método da roleta viciada, onde a probabilidade de seleção é proporcional ao fitness do indivíduo. O crossover é realizado escolhendo um ponto de corte aleatório e combinando partes dos pais. A mutação é aplicada aleatoriamente em cada indivíduo, trocando duas cidades de posição com uma probabilidade definida.

O algoritmo é executado por um número de gerações especificado, gerando uma nova população a cada iteração. No final, o algoritmo retorna a melhor rota encontrada.

No exemplo de utilização fornecido, o algoritmo é aplicado a um conjunto de cinco cidades com coordenadas específicas. O tamanho da população é definido como 100, a taxa de mutação como 0.01 e o número de gerações como 1000. A melhor rota encontrada é impressa no console.