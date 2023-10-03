Claro! Aqui está um exemplo de um código complexo em F# que envolve o uso de funções de alta ordem e manipulação de listas:

```fsharp
// Função para calcular o fatorial de um número
let rec fatorial n =
    match n with
    | 0 -> 1
    | _ -> n * fatorial (n-1)

// Função para gerar uma lista de números fatoriais de 0 até n
let listaFatoriais n =
    let rec gerarListaFatoriaisAux i acc =
        if i <= n then
            gerarListaFatoriaisAux (i+1) (fatorial i :: acc)
        else
            List.rev acc
    gerarListaFatoriaisAux 0 []

// Função de alta ordem para filtrar os números de uma lista que são divisíveis por um valor
let filtrarDivisiveisPor valor lista =
    List.filter (fun x -> x % valor = 0) lista

// Função para imprimir uma lista de números
let imprimirLista lista =
    printfn "%A" lista

// Função principal que realiza todos os passos
let main n valor =
    let lista = listaFatoriais n
    let listaFiltrada = filtrarDivisiveisPor valor lista
    imprimirLista listaFiltrada

// Chamada da função principal com o valor de n e valor definidos
main 10 3
```

Neste código, temos as seguintes funções:

1. A função `fatorial` é uma função recursiva que calcula o fatorial de um número. Ela recebe um número `n` e retorna o fatorial desse número. Ela utiliza o casamento de padrões (`match`) para definir o caso base quando `n` é igual a 0 e o caso recursivo para os demais valores.

2. A função `listaFatoriais` é uma função que recebe um número `n` e retorna uma lista com os números fatoriais de 0 até `n`. Ela utiliza uma função auxiliar `gerarListaFatoriaisAux` que faz recursão de cauda para construir a lista de forma eficiente. A lista é construída adicionando o fatorial de `i` na cabeça da lista `acc` e incrementando `i` até `n`.

3. A função `filtrarDivisiveisPor` é uma função de alta ordem que recebe um valor `valor` e uma lista de números `lista`. Ela utiliza a função `List.filter` para filtrar os números da lista que são divisíveis por `valor`.

4. A função `imprimirLista` é uma função que recebe uma lista de números `lista` e imprime essa lista utilizando a função `printfn`.

5. A função `main` é a função principal que realiza todos os passos. Ela recebe dois parâmetros `n` e `valor`, chama a função `listaFatoriais` para gerar a lista de fatoriais, a função `filtrarDivisiveisPor` para filtrar os números divisíveis por `valor` e a função `imprimirLista` para imprimir o resultado final.

O resultado final será a lista de números fatoriais de 0 até `n` que são divisíveis por `valor`.