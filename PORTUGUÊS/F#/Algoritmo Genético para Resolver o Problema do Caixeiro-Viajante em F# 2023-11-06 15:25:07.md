Claro! Aqui está um exemplo de código complexo em F#, que implementa um algoritmo genético para resolver o problema clássico do caixeiro-viajante:

```fsharp
open System

// Definindo o tipo de dados representando um indivíduo
type Individuo =
    {
        cromossomo: int array;
        fitness: float;
    }

// Função para calcular a distância entre duas cidades
let distanciaEntreCidades (cidadeA: int, cidadeB: int) =
    // Implemente aqui a lógica para calcular a distância entre as cidades
    // (por exemplo, utilizando coordenadas geográficas)

// Função para calcular a aptidão de um indivíduo
let calcularAptidao (individuo: Individuo) =
    let mutable aptidao = 0.0
    for i = 0 to Array.length individuo.cromossomo - 2 do
        let cidadeAtual = individuo.cromossomo.[i]
        let proximaCidade = individuo.cromossomo.[i+1]
        aptidao <- aptidao + distanciaEntreCidades cidadeAtual proximaCidade
    individuo.fitness <- 1.0 / aptidao

// Função para criar uma população inicial
let criarPopulacaoInicial (tamanhoPopulacao: int, numeroCidades: int) =
    let rnd = new Random()
    let populacao =
        Array.init tamanhoPopulacao (fun _ ->
            let cromossomo =
                Array.init numeroCidades (fun _ -> rnd.Next(0, numeroCidades))
            let fitness = 0.0
            { cromossomo = cromossomo; fitness = fitness }
        )
    populacao

// Função para selecionar os melhores indivíduos da população
let selecionarMelhores (populacao: Individuo array, percentual: float) =
    let numeroMelhores = int (percentual * float (Array.length populacao))
    let melhores =
        Array.sortByDescending (fun individuo -> individuo.fitness) populacao
        |> Array.take numeroMelhores
    melhores

// Função para realizar o crossover entre dois indivíduos
let crossover (pai: Individuo, mae: Individuo) =
    let numeroCidades = Array.length pai.cromossomo
    let filho =
        Array.init numeroCidades (fun i ->
            if i < numeroCidades / 2 then
                pai.cromossomo.[i]
            else
                mae.cromossomo.[i]
        )
    { cromossomo = filho; fitness = 0.0 }

// Função para realizar a mutação em um indivíduo
let mutacao (individuo: Individuo, taxaMutacao: float) =
    for i = 0 to Array.length individuo.cromossomo - 1 do
        if Random().NextDouble() < taxaMutacao then
            individuo.cromossomo.[i] <- Random().Next(0, Array.length individuo.cromossomo)

// Função para evoluir a população em uma geração
let evoluirPopulacao (populacao: Individuo array, taxaCrossover: float, taxaMutacao: float) =
    let melhores = selecionarMelhores populacao 0.2
    let novaPopulacao =
        [| for _ in 0 .. Array.length populacao - 1 -> crossover (melhores.[Random().Next(0, Array.length melhores)], melhores.[Random().Next(0, Array.length melhores)]) |]
    Array.iter (mutacao >> calcularAptidao) novaPopulacao
    melhores @ novaPopulacao

// Função principal para resolver o problema do caixeiro-viajante utilizando algoritmo genético
let resolverCaixeiroViajante (numeroCidades: int, tamanhoPopulacao: int, numeroGeracoes: int, taxaCrossover: float, taxaMutacao: float) =
    let mutable populacao = criarPopulacaoInicial tamanhoPopulacao numeroCidades
    for _ in 1 .. numeroGeracoes do
        Array.iter calcularAptidao populacao
        populacao <- evoluirPopulacao populacao taxaCrossover taxaMutacao
    let melhorIndividuo = Array.maxBy (fun individuo -> individuo.fitness) populacao
    melhorIndividuo
```

Este código implementa um algoritmo genético para resolver o problema do caixeiro-viajante. Ele começa definindo um tipo de dados `Individuo`, que representa um indivíduo na população. Cada indivíduo possui um cromossomo (representado por um array de inteiros) e uma aptidão (fitness), que é calculada com base na distância percorrida pelo caixeiro-viajante.

A função `distanciaEntreCidades` é responsável por calcular a distância entre duas cidades. Neste exemplo, ela está definida como uma função genérica, e você deve implementar a lógica específica para o seu problema.

A função `calcularAptidao` calcula a aptidão de um indivíduo com base no seu cromossomo. Ela percorre o cromossomo, calculando a distância entre cada par de cidades, e em seguida atualiza a aptidão do indivíduo.

A função `criarPopulacaoInicial` gera uma população inicial de indivíduos com cromossomos aleatórios.

A função `selecionarMelhores` seleciona os melhores indivíduos da população com base no seu fitness. Neste exemplo, ela seleciona os 20% melhores indivíduos.

A função `crossover` realiza o crossover entre dois indivíduos, gerando um novo indivíduo como filho.

A função `mutacao` realiza a mutação em um indivíduo, alterando aleatoriamente alguns genes do seu cromossomo.

A função `evoluirPopulacao` evolui a população em uma geração. Ela seleciona os melhores indivíduos, realiza o crossover entre eles, e em seguida aplica a mutação nos novos indivíduos gerados.

Por fim, a função `resolverCaixeiroViajante` é a função principal que utiliza todas as funções anteriores para resolver o problema do caixeiro-viajante. Ela recebe como parâmetros o número de cidades, o tamanho da população, o número de gerações, a taxa de crossover e a taxa de mutação. Ela retorna o melhor indivíduo encontrado após as gerações especificadas.

Este código é apenas um exemplo básico de como implementar um algoritmo genético em F#. Você pode personalizá-lo de acordo com as suas necessidades e adaptá-lo para resolver outros problemas.