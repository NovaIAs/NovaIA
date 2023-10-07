Claro! Aqui está um exemplo de código complexo em F#:

```fsharp
open System

// Define um tipo de dado para representar uma pessoa
type Pessoa = {
    Nome: string
    Idade: int
    Sexo: string
}

// Define uma função para calcular a média de idade das pessoas de um determinado sexo
let calcularMediaIdade (pessoas: Pessoa list) (sexo: string) =
    let pessoasFiltradas = pessoas |> List.filter (fun p -> p.Sexo = sexo)
    let totalIdades = pessoasFiltradas |> List.map (fun p -> p.Idade) |> List.sum
    let quantidadePessoas = pessoasFiltradas |> List.length

    if quantidadePessoas > 0 then
        totalIdades / quantidadePessoas
    else
        0

// Função principal
[<EntryPoint>]
let main argv =

    // Cria uma lista de pessoas
    let pessoas = [
        { Nome = "João"; Idade = 25; Sexo = "Masculino" }
        { Nome = "Maria"; Idade = 30; Sexo = "Feminino" }
        { Nome = "Pedro"; Idade = 40; Sexo = "Masculino" }
        { Nome = "Ana"; Idade = 35; Sexo = "Feminino" }
        { Nome = "Carlos"; Idade = 28; Sexo = "Masculino" }
    ]

    // Calcula a média de idade das pessoas do sexo masculino
    let mediaIdadeMasculino = calcularMediaIdade pessoas "Masculino"

    // Calcula a média de idade das pessoas do sexo feminino
    let mediaIdadeFeminino = calcularMediaIdade pessoas "Feminino"

    // Imprime os resultados na tela
    printfn "Média de idade do sexo masculino: %d" mediaIdadeMasculino
    printfn "Média de idade do sexo feminino: %d" mediaIdadeFeminino

    0 // Retorna 0 para indicar que o programa foi executado com sucesso
```

Este exemplo de código em F# define um tipo de dado chamado `Pessoa`, que possui os campos `Nome`, `Idade` e `Sexo`. Em seguida, é definida a função `calcularMediaIdade`, que recebe uma lista de pessoas e um sexo como parâmetros e calcula a média de idade das pessoas desse sexo.

Na função principal (`main`), é criada uma lista de pessoas com diferentes idades e sexos. Em seguida, a função `calcularMediaIdade` é chamada duas vezes, uma para calcular a média de idade do sexo masculino e outra para calcular a média de idade do sexo feminino.

Por fim, os resultados são impressos na tela usando a função `printfn`. O programa retorna 0 para indicar que foi executado com sucesso.

Espero que este exemplo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.