Claro! Aqui está um exemplo complexo de código em Haskell:

```haskell
module Main where

import Data.List

type Telefone = (String, String)

type Agenda = [Telefone]

-- Função para adicionar um novo telefone à agenda
adicionarTelefone :: Agenda -> Telefone -> Agenda
adicionarTelefone agenda novoTelefone = novoTelefone : agenda

-- Função para remover um telefone da agenda
removerTelefone :: Agenda -> String -> Agenda
removerTelefone agenda nome = filter (\(n, _) -> n /= nome) agenda

-- Função para atualizar o número de um telefone na agenda
atualizarTelefone :: Agenda -> String -> String -> Agenda
atualizarTelefone agenda nome novoNumero = map (\(n, num) -> if n == nome then (n, novoNumero) else (n, num)) agenda

-- Função para buscar todos os telefones de um determinado nome
buscarTelefonesPorNome :: Agenda -> String -> [Telefone]
buscarTelefonesPorNome agenda nome = filter (\(n, _) -> n == nome) agenda

-- Função para buscar todos os nomes que possuem um determinado número
buscarNomesPorTelefone :: Agenda -> String -> [String]
buscarNomesPorTelefone agenda numero = map fst $ filter (\(_, num) -> num == numero) agenda

-- Função para ordenar a agenda por ordem alfabética de nomes
ordenarPorNome :: Agenda -> Agenda
ordenarPorNome agenda = sortOn fst agenda

-- Função para imprimir a agenda
imprimirAgenda :: Agenda -> IO ()
imprimirAgenda agenda = mapM_ (\(nome, telefone) -> putStrLn (nome ++ ": " ++ telefone)) agenda

-- Função principal
main :: IO ()
main = do
    let agendaInicial = [("Alice", "1111"), ("Bruno", "2222"), ("Carla", "3333")]
    putStrLn "Agenda inicial:"
    imprimirAgenda agendaInicial

    let agenda1 = adicionarTelefone agendaInicial ("Daniel", "4444")
    putStrLn "\nAdicionando telefone:"
    imprimirAgenda agenda1

    let agenda2 = removerTelefone agenda1 "Bruno"
    putStrLn "\nRemovendo telefone:"
    imprimirAgenda agenda2

    let agenda3 = atualizarTelefone agenda2 "Carla" "5555"
    putStrLn "\nAtualizando telefone:"
    imprimirAgenda agenda3

    let agenda4 = buscarTelefonesPorNome agenda3 "Alice"
    putStrLn "\nBuscando telefones por nome:"
    imprimirAgenda agenda4

    let nomes = buscarNomesPorTelefone agenda3 "5555"
    putStrLn "\nBuscando nomes por telefone:"
    mapM_ putStrLn nomes

    let agendaFinal = ordenarPorNome agenda3
    putStrLn "\nOrdenando por nome:"
    imprimirAgenda agendaFinal
```

Neste exemplo, implementei uma agenda telefônica em Haskell. A agenda é representada por uma lista de tuplas, onde cada tupla contém um nome e um número de telefone.

As principais funções implementadas são:

- `adicionarTelefone`: adiciona um novo telefone à agenda.
- `removerTelefone`: remove um telefone da agenda pelo nome.
- `atualizarTelefone`: atualiza o número de um telefone na agenda pelo nome.
- `buscarTelefonesPorNome`: busca todos os telefones de um determinado nome na agenda.
- `buscarNomesPorTelefone`: busca todos os nomes que possuem um determinado número de telefone na agenda.
- `ordenarPorNome`: ordena a agenda por ordem alfabética de nomes.
- `imprimirAgenda`: imprime a agenda na saída padrão.

A função `main` é a função principal, que faz uso das funções implementadas para realizar operações na agenda. Neste exemplo, a agenda inicial é definida com três contatos e são realizadas operações como adicionar telefone, remover telefone, atualizar telefone, buscar telefones por nome, buscar nomes por telefone e ordenar a agenda por nome.

Espero que isso atenda às suas necessidades! Se você tiver alguma dúvida ou precisar de mais ajuda, sinta-se à vontade para perguntar.