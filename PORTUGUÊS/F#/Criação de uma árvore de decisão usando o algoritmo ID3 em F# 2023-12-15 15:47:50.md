Aqui está um código em F# que cria uma árvore de decisão usando o algoritmo ID3 para classificação de dados. Ele é um código complexo e diferenciado em relação a outros exemplos de árvores de decisão que normalmente são mostrados.

```fsharp
open System
open System.Collections.Generic

type Valor = 
    | Numerico of float
    | Categorico of string

type Exemplo = {
    Atributos: Valor list
    Classe: string
}

type ArvoreDecisao =
    | Folha of string
    | No of string * (Valor -> ArvoreDecisao)

let contaValores (exemplos: Exemplo list) (atributo: int) = 
    let valores = new Dictionary<Valor, int>()
    for exemplo in exemplos do
        let valor = List.item atributo exemplo.Atributos
        match valores.TryGetValue(valor) with
        | true, count -> valores.[valor] <- count + 1
        | _ -> valores.Add(valor, 1)
    valores

let entropia (exemplos: Exemplo list) =
    let total = exemplos.Length
    let classeCounts = contaValores exemplos (List.length exemplos.[0].Atributos - 1)
    let ent = classeCounts |> Seq.values |> Seq.map (fun count -> -(float count / float total) * log(float count / float total, 2.0))
    Seq.sum ent

let ganhoInformacao (exemplos: Exemplo list) (atributo: int) =
    let total = exemplos.Length
    let atributoCounts = contaValores exemplos atributo
    let ganho = ref (entropia exemplos)
    for kvp in atributoCounts do
        let (_, count) = kvp
        let subset = exemplos |> List.filter (fun exemplo -> List.item atributo exemplo.Atributos = fst kvp)
        ganho := !ganho - (float count / float total) * entropia subset
    !ganho

let atributoMaiorGanho (exemplos: Exemplo list) (atributos: int list) =
    let ganhos = atributos |> List.map (fun atributo -> atributo, ganhoInformacao exemplos atributo)
    let (_, maiorGanho) = ganhos |> List.maxBy snd
    let atributosComMaiorGanho = ganhos |> List.filter (fun (_, ganho) -> ganho = maiorGanho) |> List.map fst
    atributosComMaiorGanho |> List.head

let construirArvore (exemplos: Exemplo list) (atributos: int list) =
    let classeCounts = contaValores exemplos (List.length exemplos.[0].Atributos - 1)
    match Seq.length classeCounts with
    | 1 -> Folha (classeCounts |> Seq.head |> fst)
    | 0 -> failwith "Nenhum exemplo fornecido!"
    | _ ->
        let melhorAtributo = atributoMaiorGanho exemplos atributos
        let atributosRestantes = atributos |> List.filter (fun atributo -> atributo <> melhorAtributo)
        let atributoValor = List.nth exemplos.[0].Atributos melhorAtributo
        let subset = exemplos |> List.filter (fun exemplo -> List.item melhorAtributo exemplo.Atributos = atributoValor)
        let filhos = atributoValor
                     |> function
                        | Numerico _ -> failwith "Atributo numérico não suportado!"
                        | Categorico _ -> atributoValor |> Seq.cast<Categorico>
                                                          |> Seq.map (fun (Categorico valor) ->
                                                                     valor,
                                                                     construirArvore (subset |> List.filter (fun exemplo -> List.item melhorAtributo exemplo.Atributos = Categorico valor))
                                                                                     atributosRestantes)
                                                          |> Map.ofSeq
        No (string atributoValor, filhos)

let rec classificar (exemplo: Exemplo) (arvore: ArvoreDecisao) =
    match arvore with
    | Folha classe -> classe
    | No (_, filhos) -> exemplo.Atributos
                        |> List.nth (Map.keys filhos |> Seq.head)
                        |> function
                            | Numerico _ -> failwith "Atributo numérico não suportado!"
                            | Categorico valor -> classificar exemplo (Map.find valor filhos)

let exemplos = [
    { Atributos = [Categorico "Ensolarado"; Numerico 85.0; Numerico 85.0; Categorico "Não"; Categorico "Não"]; Classe = "Não" }
    { Atributos = [Categorico "Ensolarado"; Numerico 80.0; Numerico 90.0; Categorico "Sim"; Categorico "Não"]; Classe = "Não" }
    { Atributos = [Categorico "Nublado"; Numerico 83.0; Numerico 86.0; Categorico "Não"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Chuvoso"; Numerico 70.0; Numerico 96.0; Categorico "Não"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Chuvoso"; Numerico 68.0; Numerico 80.0; Categorico "Não"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Chuvoso"; Numerico 65.0; Numerico 70.0; Categorico "Sim"; Categorico "Não"]; Classe = "Não" }
    { Atributos = [Categorico "Nublado"; Numerico 64.0; Numerico 65.0; Categorico "Sim"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Ensolarado"; Numerico 72.0; Numerico 95.0; Categorico "Não"; Categorico "Não"]; Classe = "Não" }
    { Atributos = [Categorico "Ensolarado"; Numerico 69.0; Numerico 70.0; Categorico "Não"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Chuvoso"; Numerico 75.0; Numerico 80.0; Categorico "Não"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Ensolarado"; Numerico 75.0; Numerico 70.0; Categorico "Sim"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Ensolarado"; Numerico 72.0; Numerico 90.0; Categorico "Sim"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Nublado"; Numerico 81.0; Numerico 75.0; Categorico "Não"; Categorico "Sim"]; Classe = "Sim" }
    { Atributos = [Categorico "Nublado"; Numerico 71.0; Numerico 91.0; Categorico "Sim"; Categorico "Não"]; Classe = "Sim" }
    { Atributos = [Categorico "Chuvoso"; Numerico 71.0; Numerico 80.0; Categorico "Sim"; Categorico "Sim"]; Classe = "Não" }
]

let atributos = [0 .. List.length exemplos.[0].Atributos - 2]

let arvore = construirArvore exemplos atributos

let exemploClassificar = { Atributos = [Categorico "Chuvoso"; Numerico 72.0; Numerico 95.0; Categorico "Sim"; Categorico "Não"]; Classe = "" }

let classeExemploClassificar = classificar exemploClassificar arvore

printfn "A classe do exemplo a ser classificado é: %s" classeExemploClassificar
```

Este código implementa o algoritmo ID3 para construção de uma árvore de decisão a partir de um conjunto de exemplos. Ele usa os conceitos de entropia e ganho de informação para escolher o melhor atributo a ser utilizado como nodo da árvore. A árvore de decisão resultante é então usada para classificar novos exemplos.

Primeiro, definimos alguns tipos auxiliares, como `Valor` e `Exemplo`, que representam os valores dos atributos e os exemplos de treinamento, respectivamente. Em seguida, definimos o tipo `ArvoreDecisao`, que representa os nodos da árvore de decisão.

Em seguida, temos uma função `contaValores` que conta a frequência de cada valor de um determinado atributo nos exemplos de treinamento. A função `entropia` calcula a entropia do conjunto de exemplos fornecido. A função `ganhoInformacao` calcula o ganho de informação de um determinado atributo em relação aos exemplos de treinamento. A função `atributoMaiorGanho` retorna o atributo com o maior ganho de informação. A função `construirArvore` constrói a árvore de decisão recursivamente.

A função `classificar` é usada para classificar um exemplo fornecido usando a árvore de decisão. No final, temos uma lista de exemplos de treinamento e uma lista de atributos. Esses valores são usados para construir a árvore de decisão e, em seguida, um exemplo é classificado usando essa árvore.

Espero que este código atenda às suas expectativas!