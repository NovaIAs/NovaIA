Claro! Aqui está um código complexo em F# que você pode utilizar como base para o seu projeto:

```
open System

type Pessoa = { Nome: string; Idade: int }

let listaPessoas = [
    { Nome = "João"; Idade = 25 };
    { Nome = "Maria"; Idade = 30 };
    { Nome = "Pedro"; Idade = 40 }
]

let filtrarPessoasMaisVelhas idade pessoas =
    pessoas
    |> List.filter (fun pessoa -> pessoa.Idade > idade)

let exibirPessoas pessoas =
    pessoas
    |> List.iter (fun pessoa -> printfn "Nome: %s | Idade: %d" pessoa.Nome pessoa.Idade)

let idadeMinima = 30

let pessoasMaisVelhas = filtrarPessoasMaisVelhas idadeMinima listaPessoas

exibirPessoas pessoasMaisVelhas
```

Explicação do código:
- Primeiramente, importamos o namespace System para utilizar as funções de entrada/saída.
- Em seguida, definimos um tipo de dados chamado "Pessoa" que possui dois campos: "Nome" (string) e "Idade" (int).
- Criamos uma lista chamada "listaPessoas" com algumas pessoas e suas respectivas idades.
- Em seguida, definimos uma função chamada "filtrarPessoasMaisVelhas", que recebe um valor mínimo de idade e uma lista de pessoas, e retorna uma nova lista contendo apenas as pessoas cuja idade é maior que o valor mínimo.
- Também temos uma função chamada "exibirPessoas" que recebe uma lista de pessoas e exibe o nome e a idade de cada pessoa utilizando a função printfn do F#.
- Definimos uma variável chamada "idadeMinima" com o valor 30.
- Utilizamos a função "filtrarPessoasMaisVelhas" passando a idade mínima e a lista de pessoas para obter uma nova lista contendo apenas as pessoas mais velhas.
- Por fim, utilizamos a função "exibirPessoas" passando a lista de pessoas mais velhas para exibir o nome e a idade de cada uma delas.

Esse código é apenas um exemplo para ilustrar a utilização de tipos, funções e manipulação de listas em F#. Ele filtra e exibe as pessoas que possuem uma idade maior que um valor mínimo definido. Lembre-se que você pode adaptar e modificar esse código conforme as necessidades do seu projeto.