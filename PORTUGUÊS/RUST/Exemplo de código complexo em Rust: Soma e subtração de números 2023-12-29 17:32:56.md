Claro! Aqui está um exemplo de código complexo em Rust:

```rust
use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let numbers: Vec<i32> = input
        .trim()
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect();

    let result = calculate(numbers);
    println!("O resultado é: {}", result);
}

fn calculate(numbers: Vec<i32>) -> i32 {
    let mut sum = 0;

    for (index, number) in numbers.iter().enumerate() {
        if index % 2 == 0 {
            sum += number;
        } else {
            sum -= number;
        }
    }

    sum
}
```

Nesse código em Rust, demos um exemplo de um programa que lê uma string de entrada contendo uma sequência de números separados por espaços. Em seguida, convertemos essa string em um vetor de números inteiros utilizando a função `parse()`.

Depois, chamamos a função `calculate()` passando o vetor de números como argumento. Essa função percorre o vetor e realiza uma operação de soma ou subtração, dependendo do índice do elemento no vetor. Se o índice for par, somamos o número à variável `sum`, caso contrário, subtraímos.

Por fim, imprimimos o resultado na tela utilizando a função `println!()`.