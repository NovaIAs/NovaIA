Claro! Abaixo está um código complexo em F# que implementa um algoritmo genético para resolver o Problema do Caixeiro Viajante (TSP - Traveling Salesman Problem). O código está totalmente em português e possui comentários explicativos para ajudar na compreensão:

```
open System

// Definindo a estrutura de um cromossomo
type Cromossomo = { rota: int list; fitness: float }

// Função para calcular a distância entre duas cidades
let calcularDistancia cidade1 cidade2 =
    let dx = cidade2.X - cidade1.X
    let dy = cidade2.Y - cidade1.Y
    sqrt (float (dx * dx + dy * dy))

// Função para calcular o fitness de um cromossomo (distância total percorrida)
let calcularFitness cromossomo cidades =
    let rec calcularDistanciaTotal rota =
        match rota with
        | [] -> 0.0
        | cidade1 :: cidade2 :: tail ->
            let distancia = calcularDistancia cidades.[cidade1] cidades.[cidade2]
            distancia + calcularDistanciaTotal (cidade2 :: tail)
        | _ -> failwith "Rota inválida"
    { cromossomo with fitness = calcularDistanciaTotal cromossomo.rota }

// Função para criar uma população inicial de cromossomos
let criarPopulacaoInicial tamanhoPopulacao numCidades =
    let random = Random()
    let criarCromossomo _ =
        let rota = List.permute [ 0..numCidades-1 ]
        let fitness = 0.0
        { rota = rota; fitness = fitness }
    Array.init tamanhoPopulacao criarCromossomo

// Função para selecionar dois pais usando o método da roleta viciada
let selecionarPais populacao =
    let totalFitness = Array.sumBy (fun cromossomo -> int (cromossomo.fitness)) populacao
    let random = Random()
    let rec selecionarPai acumulador indice =
        let novoAcumulador = acumulador + (int (populacao.[indice].fitness))
        if novoAcumulador >= random.Next(totalFitness) then populacao.[indice]
        else selecionarPai novoAcumulador (indice + 1)
    (selecionarPai 0 0, selecionarPai 0 0)

// Função para realizar o crossover entre dois pais para gerar dois filhos
let crossover pai1 pai2 =
    let pontoCorte = Random().Next(pai1.rota.Length)
    let rec gerarFilho filhos pai1 pai2 pontoCorte =
        let filho = Array.create pai1.rota.Length (-1)
        let rec preencherFilho indicePai1 indicePai2 =
            if indicePai1 < pontoCorte then
                let cidade = pai1.rota.[indicePai1]
                if not (Array.contains cidade filho) then
                    filho.[indicePai1] <- cidade
                    preencherFilho (indicePai1 + 1) indicePai2
                else
                    preencherFilho (indicePai1 + 1) indicePai2
            else
                let cidade = pai2.rota.[indicePai2]
                if not (Array.contains cidade filho) then
                    let indiceFilho = Array.findIndex (fun x -> x = -1) filho
                    filho.[indiceFilho] <- cidade
                    preencherFilho indicePai1 (indicePai2 + 1)
                else
                    preencherFilho indicePai1 (indicePai2 + 1)
        preencherFilho 0 0
        { rota = filho |> Array.toList; fitness = 0.0 }
    (gerarFilho pai1 pai2 pontoCorte, gerarFilho pai2 pai1 pontoCorte)

// Função para realizar a mutação em um cromossomo
let mutacao cromossomo taxaMutacao =
    let random = Random()
    let rec realizarMutacao rota =
        let tamanhoRota = rota.Length
        let indice1 = random.Next(tamanhoRota)
        let indice2 = random.Next(tamanhoRota)
        let novaRota =
            rota
            |> List.mapi (fun i cidade -> if i = indice1 then rota.[indice2] else if i = indice2 then rota.[indice1] else cidade)
        if random.NextDouble() <= taxaMutacao then novaRota |> realizarMutacao
        else novaRota
    { cromossomo with rota = realizarMutacao cromossomo.rota }

// Função para encontrar o melhor cromossomo da população
let encontrarMelhorCromossomo populacao =
    Array.minBy (fun cromossomo -> int (cromossomo.fitness)) populacao

// Função principal do algoritmo genético
let algoritmoGenetico numCidades tamanhoPopulacao taxaMutacao numGeracoes =
    let random = Random()
    let cidades =
        Array.init numCidades (fun _ ->
            { X = random.NextDouble(); Y = random.NextDouble() }) // Gerando coordenadas aleatórias para as cidades
    let mutable populacao = criarPopulacaoInicial tamanhoPopulacao numCidades
    let rec iterar geracao =
        if geracao = numGeracoes then
            populacao |> encontrarMelhorCromossomo
        else
            populacao <-
                Array.init tamanhoPopulacao (fun _ ->
                    let pai1, pai2 = selecionarPais populacao
                    let filho1, filho2 = crossover pai1 pai2
                    [ filho1; filho2 ]
                    |> List.map (fun filho -> mutacao filho taxaMutacao))
            iterar (geracao + 1)
    iterar 0

// Exemplo de uso do algoritmo genético com 10 cidades, população de 1000 indivíduos,
// taxa de mutação de 5% e 100 gerações
let resultado = algoritmoGenetico 10 1000 0.05 100
printfn "Melhor cromossomo encontrado: %A" resultado.rota
```

Este código implementa um algoritmo genético, utilizando F#, para resolver o Problema do Caixeiro Viajante. O algoritmo começa gerando uma população inicial de cromossomos, onde cada cromossomo representa uma possível rota a ser percorrida pelo caixeiro. Em seguida, o algoritmo realiza o crossover entre os pais selecionados, gerando dois filhos, e aplica uma mutação em cada filho. Esses passos são repetidos por um número pré-determinado de gerações, até que o melhor cromossomo seja encontrado. O resultado final é a melhor rota encontrada pelo algoritmo.

Vale ressaltar que este código é apenas um exemplo e pode ser adaptado e melhorado para atender a diferentes requisitos e problemas específicos.