Certamente! Aqui está um código em F# que implementa um algoritmo genético para resolver o problema do caixeiro viajante (TSP).

```fsharp
open System

// Define a estrutura que representa uma cidade
type Cidade = {
    Nome: string
    Coordenadas: float * float
}

// Define a estrutura que representa um indivíduo da população
type Individuo = {
    Rota: Cidade list
    Fitness: float
}

// Função para calcular a distância euclidiana entre duas cidades
let calcularDistancia (cidade1: Cidade) (cidade2: Cidade) =
    let x1, y1 = cidade1.Coordenadas
    let x2, y2 = cidade2.Coordenadas
    sqrt ((x2 - x1) ** 2.0 + (y2 - y1) ** 2.0)

// Função para calcular o fitness de um indivíduo
let calcularFitness (individuo: Individuo) =
    let rec loop rota acc =
        match rota with
        | [] -> acc
        | [cidade] -> acc + calcularDistancia cidade (List.head individuo.Rota)
        | cidade1::cidade2::resto ->
            let distancia = calcularDistancia cidade1 cidade2
            loop (cidade2::resto) (acc + distancia)
    let rotaCompleta = individuo.Rota @ [List.head individuo.Rota]
    individuo.Fitness <- loop rotaCompleta 0.0

// Função para gerar uma população inicial aleatória
let gerarPopulacaoInicial (cidades: Cidade list) (tamanhoPopulacao: int) =
    let gerarRotaAleatoria () =
        cidades |> List.permute |> List.take (List.length cidades)
    List.init tamanhoPopulacao (fun _ -> { Rota = gerarRotaAleatoria (); Fitness = 0.0 })

// Função para selecionar os melhores indivíduos da população
let selecionarMelhoresIndividuos (populacao: Individuo list) (numSelecionados: int) =
    populacao |> List.sortBy (fun individuo -> individuo.Fitness) |> List.take numSelecionados

// Função para fazer o cruzamento de dois indivíduos
let cruzarIndividuos (individuo1: Individuo) (individuo2: Individuo) =
    let pontoCorte = Random().Next(0, List.length individuo1.Rota)
    let rotaFilho1 = individuo1.Rota |> List.take pontoCorte
    let rotaFilho2 = individuo2.Rota |> List.skip pontoCorte |> List.append rotaFilho1
    { Rota = rotaFilho1; Fitness = 0.0 }, { Rota = rotaFilho2; Fitness = 0.0 }

// Função para fazer a mutação de um indivíduo
let mutarIndividuo (individuo: Individuo) =
    let pontoMutacao1 = Random().Next(0, List.length individuo.Rota)
    let pontoMutacao2 = Random().Next(0, List.length individuo.Rota)
    let rotaMutada =
        individuo.Rota
        |> List.mapi (fun i cidade ->
            if i = pontoMutacao1 then individuo.Rota.[pontoMutacao2]
            elif i = pontoMutacao2 then individuo.Rota.[pontoMutacao1]
            else cidade)
    { Rota = rotaMutada; Fitness = 0.0 }

// Função principal que executa o algoritmo genético
let algoritmoGenetico (cidades: Cidade list) (tamanhoPopulacao: int) (numGeracoes: int) =
    let populacao = gerarPopulacaoInicial cidades tamanhoPopulacao
    let rec loop geracao populacao =
        if geracao = numGeracoes then
            let melhorIndividuo = selecionarMelhoresIndividuos populacao 1 |> List.head
            printfn "Melhor solução encontrada:"
            printfn "Rota: %A" melhorIndividuo.Rota
            printfn "Distância: %.2f" melhorIndividuo.Fitness
        else
            // Avaliar fitness dos indivíduos
            List.iter calcularFitness populacao

            // Selecionar os melhores indivíduos para reprodução
            let selecionados = selecionarMelhoresIndividuos populacao (tamanhoPopulacao / 2)

            // Fazer cruzamento e mutação para gerar nova população
            let novaPopulacao =
                let rec gerarPopulacao pais acc =
                    match pais with
                    | [] -> acc
                    | [pai] -> mutarIndividuo pai :: acc
                    | pai1::pai2::resto ->
                        let filho1, filho2 = cruzarIndividuos pai1 pai2
                        gerarPopulacao resto (filho1 :: filho2 :: acc)
                gerarPopulacao selecionados []

            loop (geracao + 1) novaPopulacao

    loop 1 populacao

// Exemplo de uso
let cidades = [
    { Nome = "A"; Coordenadas = (1.0, 1.0) }
    { Nome = "B"; Coordenadas = (2.0, 3.0) }
    { Nome = "C"; Coordenadas = (5.0, 2.0) }
    { Nome = "D"; Coordenadas = (4.0, 6.0) }
    { Nome = "E"; Coordenadas = (3.0, 4.0) }
]
let tamanhoPopulacao = 100
let numGeracoes = 1000
algoritmoGenetico cidades tamanhoPopulacao numGeracoes
```

Neste código, eu criei uma estrutura `Cidade` para representar cada cidade, com um nome e suas coordenadas. Em seguida, temos a estrutura `Individuo`, que representa um indivíduo da população, com sua rota (uma lista de cidades) e seu fitness (a distância total percorrida).

A função `calcularDistancia` calcula a distância euclidiana entre duas cidades, utilizando suas coordenadas. A função `calcularFitness` calcula o fitness de um indivíduo, percorrendo sua rota e somando as distâncias entre as cidades.

A função `gerarPopulacaoInicial` gera uma população inicial aleatória, utilizando a função `permute` para embaralhar a lista de cidades.

A função `selecionarMelhoresIndividuos` seleciona os melhores indivíduos da população, com base em seus fitness, retornando uma lista com os `numSelecionados` melhores.

A função `cruzarIndividuos` realiza o cruzamento de dois indivíduos, gerando dois filhos.

A função `mutarIndividuo` realiza a mutação de um indivíduo, trocando duas cidades em posições aleatórias de sua rota.

A função `algoritmoGenetico` é a função principal que executa o algoritmo genético. Ela gera a população inicial, e em cada geração, avalia o fitness dos indivíduos, seleciona os melhores para reprodução, realiza o cruzamento e a mutação, e repete o processo até atingir o número de gerações desejado.

No exemplo de uso no final do código, temos uma lista de cidades e parâmetros para o tamanho da população e o número de gerações. Chamamos a função `algoritmoGenetico` passando essas informações para encontrar a melhor solução para o problema do caixeiro viajante.