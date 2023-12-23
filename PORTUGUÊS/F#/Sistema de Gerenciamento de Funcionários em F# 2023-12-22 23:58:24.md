Claro! Aqui está um código complexo em F# que envolve a criação de um sistema de gerenciamento de funcionários em uma empresa fictícia. O código utiliza tipos de dados, funções, operações de listas e manipulação de arquivos. Vamos lá:

```fsharp
open System.IO

// Definição do tipo de dado representando um funcionário
type Funcionario = {
    Nome: string
    Idade: int
    Cargo: string
}

// Função para adicionar um novo funcionário à lista existente
let adicionarFuncionario (funcionario: Funcionario) (listaFuncionarios: Funcionario list) =
    funcionario :: listaFuncionarios

// Função para remover um funcionário da lista existente
let removerFuncionario (nome: string) (listaFuncionarios: Funcionario list) =
    List.filter (fun f -> f.Nome <> nome) listaFuncionarios

// Função para atualizar o cargo de um funcionário
let atualizarCargo (nome: string) (novoCargo: string) (listaFuncionarios: Funcionario list) =
    List.map (fun f -> if f.Nome = nome then { f with Cargo = novoCargo } else f) listaFuncionarios

// Função para calcular a média de idade dos funcionários
let mediaIdadeFuncionarios (listaFuncionarios: Funcionario list) =
    let totalIdade = List.fold (fun acc f -> acc + f.Idade) 0 listaFuncionarios
    float totalIdade / float (List.length listaFuncionarios)

// Função para salvar a lista de funcionários em um arquivo
let salvarFuncionarios (listaFuncionarios: Funcionario list) (nomeArquivo: string) =
    use writer = new StreamWriter(nomeArquivo)
    for f in listaFuncionarios do
        writer.WriteLine(f.Nome + "," + string(f.Idade) + "," + f.Cargo)

// Função para carregar a lista de funcionários de um arquivo
let carregarFuncionarios (nomeArquivo: string) =
    if not (File.Exists(nomeArquivo)) then []
    else
        use reader = new StreamReader(nomeArquivo)
        let mutable listaFuncionarios = []
        while not reader.EndOfStream do
            let linha = reader.ReadLine()
            let dados = linha.Split(',')
            let nome = dados.[0]
            let idade = int dados.[1]
            let cargo = dados.[2]
            let novoFuncionario = { Nome = nome; Idade = idade; Cargo = cargo }
            listaFuncionarios <- novoFuncionario :: listaFuncionarios
        List.rev listaFuncionarios

// Exemplo de uso das funções
let main =
    let listaFuncionarios = carregarFuncionarios "funcionarios.txt"
    let listaAtualizada = 
        listaFuncionarios
        |> adicionarFuncionario { Nome = "João"; Idade = 30; Cargo = "Analista" }
        |> removerFuncionario "Ana"
        |> atualizarCargo "Maria" "Gerente"
    let mediaIdade = mediaIdadeFuncionarios listaAtualizada
    salvarFuncionarios listaAtualizada "funcionarios_atualizados.txt"
    printfn "Lista de Funcionários Atualizada:"
    for f in listaAtualizada do
        printfn "Nome: %s, Idade: %d, Cargo: %s" f.Nome f.Idade f.Cargo
    printfn "Média de Idade: %.2f" mediaIdade
```

Neste código, inicialmente definimos o tipo de dado `Funcionario` que representa cada funcionário, com as propriedades `Nome`, `Idade` e `Cargo`.

Em seguida, temos as funções `adicionarFuncionario`, `removerFuncionario` e `atualizarCargo`, que realizam as operações de adicionar, remover e atualizar informações de um funcionário na lista existente, respectivamente.

A função `mediaIdadeFuncionarios` calcula a média de idade dos funcionários da lista.

As funções `salvarFuncionarios` e `carregarFuncionarios` são responsáveis por salvar a lista de funcionários em um arquivo e carregar a lista a partir de um arquivo, respectivamente. O arquivo utilizado neste exemplo é "funcionarios.txt".

Por fim, no exemplo de uso no `main`, carregamos a lista de funcionários do arquivo, realizamos as operações de adicionar, remover e atualizar funcionários, calculamos a média de idade e salvamos a lista atualizada em um novo arquivo chamado "funcionarios_atualizados.txt". Em seguida, exibimos os funcionários atualizados e a média de idade.

Espero que este código atenda às suas expectativas!