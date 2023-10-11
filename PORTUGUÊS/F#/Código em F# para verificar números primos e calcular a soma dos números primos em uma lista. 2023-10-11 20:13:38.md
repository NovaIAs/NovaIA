Claro! Aqui está um exemplo de código complexo em F# que envolve a manipulação de listas:

```fsharp
// Função para verificar se um número é primo
let isPrime number =
    if number < 2 then
        false
    else
        let rec isDivisibleByAny divisor number =
            if divisor * divisor > number then
                true
            elif number % divisor = 0 then
                false
            else
                isDivisibleByAny (divisor + 1) number
        isDivisibleByAny 2 number

// Função para filtrar os números primos de uma lista de inteiros
let filterPrimes numbers =
    List.filter isPrime numbers

// Função para calcular a soma dos números primos de uma lista
let sumPrimes numbers =
    List.sum (filterPrimes numbers)

// Função principal
let main () =
    let numbers = [2; 3; 4; 5; 6; 7; 8; 9; 10]
    let primes = filterPrimes numbers
    let sumOfPrimes = sumPrimes numbers
    printfn "Números primos: %A" primes
    printfn "Soma dos números primos: %d" sumOfPrimes

main ()
```

Neste código, temos várias funções definidas. A primeira função, `isPrime`, verifica se um número é primo ou não. Ela utiliza uma função auxiliar recursiva, `isDivisibleByAny`, para verificar se o número é divisível por algum número menor que ele mesmo. Se não for divisível por nenhum número menor, então é considerado primo.

A segunda função, `filterPrimes`, recebe uma lista de números inteiros e retorna apenas os números primos presentes na lista. Ela utiliza a função `isPrime` para filtrar os números.

A terceira função, `sumPrimes`, recebe uma lista de números inteiros e calcula a soma dos números primos presentes na lista. Ela utiliza a função `filterPrimes` para obter os números primos e, em seguida, utiliza a função `List.sum` para calcular a soma desses números.

Por fim, a função `main` é a função principal do programa. Ela cria uma lista de números de exemplo, chama as funções `filterPrimes` e `sumPrimes` para obter os números primos e a soma dos números primos, respectivamente, e imprime os resultados na tela utilizando a função `printfn`.

Espero que este código complexo em F# seja útil e atenda às suas expectativas!