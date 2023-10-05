Claro! Aqui está um exemplo de um código complexo em Rust, que usa diferentes conceitos e recursos da linguagem:

```rust
use std::io::{self, BufRead};

fn main() {
    println!("Bem-vindo ao programa de cálculo de média!");

    let mut numbers = Vec::new();
    let stdin = io::stdin();

    loop {
        println!("Insira um número (ou 'q' para sair):");

        let mut input = String::new();
        stdin.lock().read_line(&mut input).unwrap();

        if input.trim() == "q" {
            break;
        }

        let number: i32 = match input.trim().parse() {
            Ok(n) => n,
            Err(_) => {
                println!("Entrada inválida. Por favor, insira um número válido.");
                continue;
            }
        };

        numbers.push(number);
    }

    let sum: i32 = numbers.iter().sum();
    let count = numbers.len();

    if count == 0 {
        println!("Nenhum número foi inserido.");
    } else {
        let average = sum as f32 / count as f32;
        println!("A média dos números inseridos é: {:.2}", average);
    }
}
```

Este programa em Rust é um cálculo de média. Ele solicita ao usuário que insira números, um por vez, até que o usuário digite 'q' para sair. Em seguida, calcula a média dos números inseridos e a exibe na saída.

O programa começa com uma mensagem de boas-vindas e, em seguida, cria um vetor vazio chamado `numbers` para armazenar os números inseridos pelo usuário. Ele também cria uma instância do objeto de entrada padrão `stdin` para ler a entrada do usuário.

Em um loop infinito, o programa solicita ao usuário que insira um número ou 'q' para sair. Ele lê a entrada do usuário e verifica se é 'q'. Se for, o loop é interrompido e o programa continua com o cálculo da média. Caso contrário, o programa tenta converter a entrada em um número inteiro (`i32`). Se a conversão for bem-sucedida, o número é adicionado ao vetor `numbers`. Caso contrário, uma mensagem de erro é exibida e o próximo número é solicitado.

Depois que o usuário sai do loop, o programa calcula a soma dos números no vetor `numbers` usando o método `iter().sum()`. Em seguida, obtém o tamanho do vetor `numbers` usando o método `len()`.

Se o tamanho for zero, o programa exibe uma mensagem informando que nenhum número foi inserido. Caso contrário, o programa calcula a média convertendo a soma para um número de ponto flutuante (`f32`) e dividindo pelo tamanho como um número de ponto flutuante. A média é formatada com duas casas decimais usando a notação `"{:.2}"` e é exibida na saída.

Espero que este exemplo tenha sido útil! Se você tiver mais alguma solicitação, por favor, me avise.