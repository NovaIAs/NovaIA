Certamente, vou gerar um código complexo em Rust para você. Este código é um exemplo de uma calculadora simples que realiza operações básicas (adição, subtração, multiplicação e divisão) com dois números fornecidos pelo usuário.

```rust
use std::io;

fn main() {
    loop {
        println!("== Calculadora em Rust ==");

        println!("Escolha uma operação:");
        println!("1. Adição");
        println!("2. Subtração");
        println!("3. Multiplicação");
        println!("4. Divisão");
        println!("0. Sair");

        let mut operation_choice = String::new();
        io::stdin().read_line(&mut operation_choice).expect("Falha ao ler a entrada");

        let operation_choice: i32 = match operation_choice.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Opção inválida! Por favor, tente novamente.");
                continue;
            }
        };

        match operation_choice {
            0 => {
                println!("Encerrando a calculadora...");
                break;
            }
            1 => {
                let (num1, num2) = get_numbers();
                let result = add(num1, num2);
                println!("Resultado: {}", result);
            }
            2 => {
                let (num1, num2) = get_numbers();
                let result = subtract(num1, num2);
                println!("Resultado: {}", result);
            }
            3 => {
                let (num1, num2) = get_numbers();
                let result = multiply(num1, num2);
                println!("Resultado: {}", result);
            }
            4 => {
                let (num1, num2) = get_numbers();
                match divide(num1, num2) {
                    Some(result) => println!("Resultado: {:.2}", result),
                    None => println!("Não é possível dividir por zero!"),
                }
            }
            _ => {
                println!("Opção inválida! Por favor, tente novamente.");
            }
        }
    }
}

fn get_numbers() -> (f64, f64) {
    let mut num1 = String::new();
    let mut num2 = String::new();

    println!("Digite o primeiro número:");
    io::stdin().read_line(&mut num1).expect("Falha ao ler a entrada");

    println!("Digite o segundo número:");
    io::stdin().read_line(&mut num2).expect("Falha ao ler a entrada");

    let num1: f64 = num1.trim().parse().expect("Entrada inválida");
    let num2: f64 = num2.trim().parse().expect("Entrada inválida");

    (num1, num2)
}

fn add(num1: f64, num2: f64) -> f64 {
    num1 + num2
}

fn subtract(num1: f64, num2: f64) -> f64 {
    num1 - num2
}

fn multiply(num1: f64, num2: f64) -> f64 {
    num1 * num2
}

fn divide(num1: f64, num2: f64) -> Option<f64> {
    if num2 != 0.0 {
        Some(num1 / num2)
    } else {
        None
    }
}
```

Este código começa com a definição da função `main()`. Dentro desta função, há um loop infinito que exibe um menu de opções para o usuário. O usuário pode escolher qual operação realizar digitando o número correspondente. Se o usuário digitar 0, o loop é interrompido e a calculadora é encerrada.

Cada operação (adição, subtração, multiplicação e divisão) é implementada como uma função separada. A função `get_numbers()` é utilizada para obter os dois números fornecidos pelo usuário. As funções `add()`, `subtract()`, `multiply()` e `divide()` realizam suas respectivas operações matemáticas e retornam o resultado.

O código utiliza a biblioteca padrão `std::io` para ler a entrada do usuário. O comando `expect()` é utilizado para tratar erros de leitura de entrada. O código também utiliza o tipo `Option<f64>` para representar a possibilidade de divisão por zero na função `divide()`.