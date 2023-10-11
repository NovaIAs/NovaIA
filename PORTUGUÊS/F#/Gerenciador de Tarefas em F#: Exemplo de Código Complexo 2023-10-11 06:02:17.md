Claro! Aqui está um exemplo de código complexo em F# que envolve a criação de uma aplicação de gerenciamento de tarefas. 

```fsharp
module GerenciadorTarefas

// Definição do tipo de dados para representar uma tarefa
type Tarefa = {
    id: int
    titulo: string
    descricao: string
    concluida: bool
}

// Função para criar uma nova tarefa
let criarTarefa (id: int) (titulo: string) (descricao: string) : Tarefa =
    {
        id = id
        titulo = titulo
        descricao = descricao
        concluida = false
    }

// Função para adicionar uma tarefa a uma lista de tarefas
let adicionarTarefa (tarefa: Tarefa) (listaTarefas: Tarefa list) : Tarefa list =
    tarefa :: listaTarefas

// Função para atualizar o status de conclusão de uma tarefa
let atualizarStatusConclusao (id: int) (listaTarefas: Tarefa list) : Tarefa list =
    let rec loop tarefas =
        match tarefas with
        | [] -> []
        | tarefa :: restante ->
            if tarefa.id = id then
                { tarefa with concluida = not tarefa.concluida } :: restante
            else
                tarefa :: loop restante
    loop listaTarefas

// Função para exibir todas as tarefas
let exibirTarefas (listaTarefas: Tarefa list) : unit =
    printfn "Lista de Tarefas:\n"
    listaTarefas 
    |> List.sortBy (fun t -> t.id)
    |> List.iter (fun t ->
        printfn "ID: %d" t.id
        printfn "Título: %s" t.titulo
        printfn "Descrição: %s" t.descricao
        printfn "Concluída: %b\n" t.concluida
    )

// Função principal
[<EntryPoint>]
let main args =
    let mutable listaTarefas = []

    // Criação de algumas tarefas de exemplo
    let tarefa1 = criarTarefa 1 "Comprar leite" "Ir ao mercado comprar leite"
    let tarefa2 = criarTarefa 2 "Pagar contas" "Realizar o pagamento das contas mensais"
    let tarefa3 = criarTarefa 3 "Estudar F#" "Dedicar 1 hora por dia para estudar F#"

    // Adicionar as tarefas criadas à lista de tarefas
    listaTarefas <- adicionarTarefa tarefa1 listaTarefas
    listaTarefas <- adicionarTarefa tarefa2 listaTarefas
    listaTarefas <- adicionarTarefa tarefa3 listaTarefas

    // Atualizar o status de conclusão de uma tarefa
    listaTarefas <- atualizarStatusConclusao 2 listaTarefas

    // Exibir todas as tarefas
    exibirTarefas listaTarefas

    0 // Retorna 0 para indicar sucesso na execução do programa
```

Neste código, criamos um módulo chamado "GerenciadorTarefas" que contém algumas funções relacionadas ao gerenciamento de tarefas. A função `criarTarefa` cria uma nova tarefa com base nos parâmetros fornecidos. A função `adicionarTarefa` adiciona uma tarefa à lista de tarefas existente. A função `atualizarStatusConclusao` atualiza o status de conclusão de uma tarefa com base no ID fornecido. A função `exibirTarefas` exibe todas as tarefas presentes na lista de tarefas.

Na função principal, criamos algumas tarefas de exemplo, as adicionamos à lista de tarefas e atualizamos o status de conclusão de uma delas. Em seguida, exibimos todas as tarefas na saída do console.

Espero que este código complexo em F# atenda às suas expectativas!