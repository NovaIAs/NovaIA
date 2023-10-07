Certamente! Aqui está um exemplo de um código complexo em Rust:

```rust
fn main() {
    // Função principal

    let mut numbers: Vec<i32> = Vec::new();
    // Declaração de um vetor mutável vazio para armazenar os números

    for i in 0..10 {
        // Loop de 0 a 9 (10 iterações)

        numbers.push(i);
        // Adiciona o número atual ao vetor
    }

    println!("Números: {:?}", numbers);
    // Imprime os números armazenados no vetor

    let sum: i32 = numbers.iter().sum();
    // Calcula a soma de todos os números do vetor

    let average = sum as f32 / numbers.len() as f32;
    // Calcula a média dos números

    println!("Soma: {}", sum);
    println!("Média: {:.2}", average);
    // Imprime a soma e a média com duas casas decimais após o ponto

    let mut prime_numbers: Vec<i32> = Vec::new();
    // Declaração de um vetor mutável vazio para armazenar os números primos

    for number in numbers {
        // Loop através de cada número no vetor

        let mut is_prime = true;
        // Variável para verificar se o número é primo

        if number <= 1 {
            // Se o número for menor ou igual a 1, não é primo
            is_prime = false;
        } else {
            for i in 2..number {
                // Loop de 2 até o número - 1

                if number % i == 0 {
                    // Se o número for divisível por algum número no intervalo, não é primo
                    is_prime = false;
                    break;
                }
            }
        }

        if is_prime {
            prime_numbers.push(number);
            // Se o número for primo, adiciona ao vetor de números primos
        }
    }

    println!("Números primos: {:?}", prime_numbers);
    // Imprime os números primos

    let largest_prime = prime_numbers.iter().max();
    match largest_prime {
        Some(&max) => println!("Maior número primo: {}", max),
        None => println!("Não há números primos."),
    }
    // Encontra e imprime o maior número primo, se existir

    let smallest_prime = prime_numbers.iter().min();
    match smallest_prime {
        Some(&min) => println!("Menor número primo: {}", min),
        None => println!("Não há números primos."),
    }
    // Encontra e imprime o menor número primo, se existir
}
```

Este código em Rust realiza as seguintes tarefas:

1. Cria um vetor vazio chamado "numbers".
2. Usa um loop para adicionar os números de 0 a 9 ao vetor.
3. Imprime os números armazenados no vetor.
4. Calcula a soma e a média dos números.
5. Cria um segundo vetor vazio chamado "prime_numbers".
6. Itera sobre cada número do vetor "numbers" e verifica se é primo.
7. Adiciona os números primos ao vetor "prime_numbers".
8. Imprime os números primos.
9. Encontra e imprime o maior número primo, se existir.
10. Encontra e imprime o menor número primo, se existir.

Esse código ilustra alguns conceitos básicos de Rust, como declaração de vetores, loops, cálculos matemáticos e manipulação de vetores. Ele também demonstra o uso de condicionais, como o `if`, e o uso de blocos `match` para lidar com possíveis valores nulos.