Claro! Aqui está um exemplo de código complexo em F# que realiza a busca binária em uma lista ordenada de números:

```fsharp
open System

let rec buscaBinaria (lista: int list) (alvo: int) (inicio: int) (fim: int) =
    if inicio > fim then
        None
    else
        let meio = (inicio + fim) / 2
        match List.item meio lista with
        | x when x = alvo -> Some meio
        | x when x > alvo -> buscaBinaria lista alvo inicio (meio - 1)
        | _ -> buscaBinaria lista alvo (meio + 1) fim

let main () =
    let lista = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19]
    let alvo = 9

    match buscaBinaria lista alvo 0 (List.length lista - 1) with
    | Some indice -> printfn "O elemento %d foi encontrado no índice %d." alvo indice
    | None -> printfn "O elemento %d não foi encontrado na lista." alvo

main ()
```

Neste código, primeiro importamos o namespace `System` para poder utilizar a função `printfn` que imprime mensagens no console.

Em seguida, definimos uma função `buscaBinaria` que recebe como parâmetros a lista ordenada de números, o alvo que estamos buscando, o índice de início e o índice de fim da busca. Essa função é recursiva e utiliza o algoritmo de busca binária.

Dentro da função `buscaBinaria`, verificamos se o `inicio` é maior que o `fim`. Se for, significa que o elemento não foi encontrado na lista e retornamos `None`. Caso contrário, calculamos o índice do meio da lista e verificamos o elemento nessa posição.

Se o elemento do meio for igual ao alvo, retornamos `Some meio`, indicando que o elemento foi encontrado no índice `meio`. Se o elemento do meio for maior que o alvo, chamamos a função `buscaBinaria` recursivamente para procurar na metade inferior da lista. Caso contrário, chamamos a função recursivamente para procurar na metade superior da lista.

Na função `main`, criamos uma lista de exemplo `[1; 3; 5; 7; 9; 11; 13; 15; 17; 19]` e definimos o alvo como `9`. Em seguida, chamamos a função `buscaBinaria` passando a lista, o alvo e os índices de início e fim. 

Por fim, utilizamos um `match` para tratar o resultado da busca. Se o resultado for `Some indice`, imprimimos a mensagem indicando que o elemento foi encontrado no índice `indice`. Caso contrário, imprimimos a mensagem indicando que o elemento não foi encontrado na lista.