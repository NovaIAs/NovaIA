Claro! Aqui está um exemplo de código em F# que cria um aplicativo de agenda pessoal:

```
open System

type Evento = { Nome: string; Data: DateTime }

let eventos = ref []

let adicionarEvento () =
    printfn "Digite o nome do evento:"
    let nome = Console.ReadLine()
    printfn "Digite a data do evento (formato: dd/mm/aaaa hh:mm):"
    let dataString = Console.ReadLine()
    match DateTime.TryParseExact(dataString, "dd/MM/yyyy HH:mm", null, System.Globalization.DateTimeStyles.None,  out var data) with
    | true -> eventos := { Nome = nome; Data = data } :: !eventos
    | false -> printfn "Data inválida, evento não adicionado."

let listarEventos () =
    printfn "Eventos agendados:"
    !eventos
    |> List.sortBy (fun evento -> evento.Data)
    |> List.iter (fun evento -> printfn "%s - %s" evento.Nome evento.Data.ToString("dd/MM/yyyy HH:mm"))

let removerEvento () =
    printfn "Digite o nome do evento a ser removido:"
    let nome = Console.ReadLine()
    eventos := !eventos |> List.filter (fun evento -> evento.Nome <> nome)

let menu () =
    printfn "=== Agenda Pessoal ==="
    printfn "1 - Adicionar evento"
    printfn "2 - Listar eventos"
    printfn "3 - Remover evento"
    printfn "0 - Sair"
    printfn "======================"
    printfn "Digite a opção desejada:"
    Console.ReadLine()

let rec loop () =
    let opcao = menu()
    match opcao with
    | "1" -> adicionarEvento(); loop()
    | "2" -> listarEventos(); loop()
    | "3" -> removerEvento(); loop()
    | "0" -> printfn "Encerrando..."; ()
    | _   -> printfn "Opção inválida!"; loop()

loop()
```

Neste código, utilizamos o F# para criar um aplicativo de agenda pessoal simples por meio de um loop principal que exibe um menu de opções. O usuário pode escolher entre adicionar eventos, listar eventos, remover eventos ou sair do programa.

Os eventos são representados pelo tipo `Evento`, que possui duas propriedades: `Nome` (string) e `Data` (DateTime). Utilizamos uma lista mutável `eventos` para armazenar os eventos adicionados.

A função `adicionarEvento` solicita ao usuário o nome e a data do evento, realiza a validação da data e adiciona o evento na lista `eventos`. Caso a data seja inválida, uma mensagem de erro é exibida.

A função `listarEventos` exibe todos os eventos agendados, ordenando-os pela data. Utilizamos a função `iter` para percorrer a lista e exibir cada evento na tela.

A função `removerEvento` solicita ao usuário o nome do evento a ser removido e remove-o da lista `eventos` utilizando a função `filter` para filtrar os eventos pelo nome.

O loop principal `loop` exibe o menu, lê a opção escolhida pelo usuário e realiza a ação correspondente. O loop se repete até que o usuário escolha a opção de sair (0).

Espero que isso ajude! Se tiver alguma dúvida adicional, sinta-se à vontade para perguntar.