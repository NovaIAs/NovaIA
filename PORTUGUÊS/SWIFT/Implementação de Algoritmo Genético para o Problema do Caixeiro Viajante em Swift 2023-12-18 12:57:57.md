Claro, vou criar um exemplo complexo em Swift para você. Este código é uma implementação de um algoritmo genético para resolver o problema do caixeiro viajante, que consiste em encontrar o caminho mais curto para percorrer uma série de cidades sem repeti-las.

```swift
import Foundation

struct Cidade {
    let nome: String
    let coordenadas: (x: Double, y: Double)
}

// Definindo as cidades
let cidadeA = Cidade(nome: "A", coordenadas: (x: 0, y: 0))
let cidadeB = Cidade(nome: "B", coordenadas: (x: 1, y: 5))
let cidadeC = Cidade(nome: "C", coordenadas: (x: 3, y: 2))
let cidadeD = Cidade(nome: "D", coordenadas: (x: 6, y: 3))
let cidadeE = Cidade(nome: "E", coordenadas: (x: 8, y: 1))
let cidadeF = Cidade(nome: "F", coordenadas: (x: 7, y: 7))

let cidades = [cidadeA, cidadeB, cidadeC, cidadeD, cidadeE, cidadeF]

// Função para calcular a distância entre duas cidades
func calcularDistancia(origem: Cidade, destino: Cidade) -> Double {
    let distanciaX = destino.coordenadas.x - origem.coordenadas.x
    let distanciaY = destino.coordenadas.y - origem.coordenadas.y
    return sqrt(pow(distanciaX, 2) + pow(distanciaY, 2))
}

// Classe representando um indivíduo (caminho)
class Individuo {
    var caminho: [Cidade]
    var distanciaTotal: Double = 0.0
    
    init(caminho: [Cidade]) {
        self.caminho = caminho
        calcularDistanciaTotal()
    }
    
    func calcularDistanciaTotal() {
        distanciaTotal = 0.0
        for i in 0..<caminho.count-1 {
            distanciaTotal += calcularDistancia(origem: caminho[i], destino: caminho[i+1])
        }
        distanciaTotal += calcularDistancia(origem: caminho.last!, destino: caminho.first!)
    }
}

// Função para gerar uma população inicial de indivíduos
func gerarPopulacaoInicial(numeroIndividuos: Int) -> [Individuo] {
    var populacaoInicial: [Individuo] = []
    for _ in 0..<numeroIndividuos {
        let caminhoAleatorio = cidades.shuffled()
        let individuo = Individuo(caminho: caminhoAleatorio)
        populacaoInicial.append(individuo)
    }
    return populacaoInicial
}

// Função para selecionar os indivíduos mais aptos
func selecionarIndividuos(populacao: [Individuo], numeroSelecionados: Int) -> [Individuo] {
    let individuosOrdenados = populacao.sorted { $0.distanciaTotal < $1.distanciaTotal }
    return Array(individuosOrdenados.prefix(numeroSelecionados))
}

// Função para cruzar dois indivíduos
func cruzarIndividuos(individuoA: Individuo, individuoB: Individuo) -> Individuo {
    let pontoCorte = Int.random(in: 1..<individuoA.caminho.count-1)
    let caminhoFilho = individuoA.caminho.prefix(pontoCorte) + individuoB.caminho.suffix(from: pontoCorte)
    return Individuo(caminho: Array(caminhoFilho))
}

// Função para realizar a mutação em um indivíduo
func mutarIndividuo(individuo: Individuo) {
    let indicesAleatorios = [Int](0..<individuo.caminho.count).shuffled().prefix(2)
    individuo.caminho.swapAt(indicesAleatorios[0], indicesAleatorios[1])
    individuo.calcularDistanciaTotal()
}

// Função para gerar a próxima geração
func gerarProximaGeracao(populacao: [Individuo], numeroSelecionados: Int, taxaMutacao: Double) -> [Individuo] {
    var novaGeracao: [Individuo] = []
    let selecionados = selecionarIndividuos(populacao: populacao, numeroSelecionados: numeroSelecionados)
    novaGeracao.append(contentsOf: selecionados)
    
    for _ in selecionados.count..<populacao.count {
        let indicePaiA = Int.random(in: 0..<selecionados.count)
        let indicePaiB = Int.random(in: 0..<selecionados.count)
        let paiA = selecionados[indicePaiA]
        let paiB = selecionados[indicePaiB]
        let filho = cruzarIndividuos(individuoA: paiA, individuoB: paiB)
        
        if Double.random(in: 0..<1) < taxaMutacao {
            mutarIndividuo(individuo: filho)
        }
        
        novaGeracao.append(filho)
    }
    
    return novaGeracao
}

// Função para encontrar o melhor indivíduo após um número de gerações
func encontrarMelhorIndividuo(numeroGeracoes: Int, numeroIndividuos: Int, numeroSelecionados: Int, taxaMutacao: Double) -> Individuo {
    var populacao = gerarPopulacaoInicial(numeroIndividuos: numeroIndividuos)
    var melhorIndividuo = populacao.min { $0.distanciaTotal < $1.distanciaTotal }!
    
    for _ in 0..<numeroGeracoes {
        populacao = gerarProximaGeracao(populacao: populacao, numeroSelecionados: numeroSelecionados, taxaMutacao: taxaMutacao)
        let novoMelhorIndividuo = populacao.min { $0.distanciaTotal < $1.distanciaTotal }!
        if novoMelhorIndividuo.distanciaTotal < melhorIndividuo.distanciaTotal {
            melhorIndividuo = novoMelhorIndividuo
        }
    }
    
    return melhorIndividuo
}

// Exemplo de uso
let melhorIndividuo = encontrarMelhorIndividuo(numeroGeracoes: 100, numeroIndividuos: 100, numeroSelecionados: 10, taxaMutacao: 0.1)
print("Melhor caminho: \(melhorIndividuo.caminho.map { $0.nome })")
print("Distância total: \(melhorIndividuo.distanciaTotal)")
```

Neste código, implementamos uma estrutura `Cidade` para representar cada cidade com seu nome e coordenadas. Em seguida, definimos as cidades que serão visitadas.

Em seguida, implementamos uma função `calcularDistancia` para calcular a distância entre duas cidades usando o teorema de Pitágoras.

Em seguida, temos a classe `Individuo`, que representa um caminho percorrido pelas cidades. O método `calcularDistanciaTotal` calcula a distância total percorrida pelo indivíduo.

Em seguida, temos a função `gerarPopulacaoInicial` para gerar uma população inicial de indivíduos com caminhos aleatórios.

Em seguida, temos a função `selecionarIndividuos` para selecionar os melhores indivíduos da população.

Em seguida, temos a função `cruzarIndividuos` para cruzar dois indivíduos e gerar um filho.

Em seguida, temos a função `mutarIndividuo` para realizar a mutação em um indivíduo, trocando a posição de duas cidades.

Em seguida, temos a função `gerarProximaGeracao` para gerar a próxima geração de indivíduos, combinando indivíduos selecionados e gerando novos indivíduos por cruzamento e mutação.

Por fim, temos a função `encontrarMelhorIndividuo` que itera por um número de gerações para encontrar o melhor indivíduo.

No exemplo de uso final, chamamos a função `encontrarMelhorIndividuo` com alguns parâmetros para encontrar o melhor caminho para visitar as cidades. O resultado é impresso no console.