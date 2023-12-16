Claro! Aqui está um código complexo em Rust que realiza uma tarefa de manipulação de dados:

```rust
use std::io::{stdin, stdout, Write};

fn main() {
    let mut input = String::new();
    print!("Digite uma frase: ");
    stdout().flush().unwrap();
    stdin().read_line(&mut input).unwrap();

    let chars = input.chars().collect::<Vec<char>>();
    let mut result = String::new();

    for (i, c) in chars.iter().enumerate() {
        let mut symbol = *c;

        if c.is_alphabetic() {
            let shift = i % 10;
            if c.is_lowercase() {
                symbol = shift_char(*c, shift, 'a', 'z');
            } else {
                symbol = shift_char(*c, shift, 'A', 'Z');
            }
        }

        result.push(symbol);
    }

    println!("Resultado: {}", result);
}

fn shift_char(c: char, shift: usize, start: char, end: char) -> char {
    let range = (end as usize - start as usize) + 1;
    let shifted = ((c as usize - start as usize + shift) % range) + start as usize;
    shifted as u8 as char
}
```

Este código em Rust lê uma frase digitada pelo usuário, realiza uma operação de deslocamento nas letras da frase e exibe o resultado.

Primeiro, o código inicia importando os módulos necessários para lidar com entrada e saída de dados. Em seguida, a função `main()` é definida como ponto de entrada do programa.

Dentro da função `main()`, declaramos uma variável `input` para armazenar a frase digitada pelo usuário. A função `print!()` é usada para exibir a mensagem "Digite uma frase: " no console. Em seguida, chamamos a função `flush()` para garantir que a mensagem seja exibida antes de obter a entrada do usuário. A função `read_line()` é usada para ler a entrada do usuário e armazená-la na variável `input`.

Em seguida, convertemos a string `input` em um vetor de caracteres usando `chars()`. Em um loop `for`, percorremos cada caractere e verificamos se é alfabético. Se for, calculamos um deslocamento com base no índice atual. Em seguida, chamamos a função `shift_char()` para realizar o deslocamento do caractere. O caractere resultante é adicionado à variável `result`.

Por fim, exibimos o resultado usando a função `println!()`.

A função `shift_char()` recebe um caractere `c`, um valor de deslocamento `shift`, e os caracteres iniciais e finais de um intervalo. Ela calcula o deslocamento aplicando a fórmula `(c - start + shift) % range + start` e retorna o caractere resultante.

Espero que este código complexo em Rust atenda às suas expectativas! Lembre-se de que posso gerar qualquer tipo de código de programação, então sinta-se à vontade para solicitar outras linguagens ou tarefas específicas.