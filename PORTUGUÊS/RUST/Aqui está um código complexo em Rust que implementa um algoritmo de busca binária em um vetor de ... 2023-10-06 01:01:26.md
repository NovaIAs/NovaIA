Claro! Aqui está um código complexo em Rust, que faz o cálculo da sequência de Fibonacci e exibe os resultados até um determinado número escolhido pelo usuário:

```rust
use std::io;

fn main() {
    println!("Bem-vindo! Este programa calcula a sequência de Fibonacci.");

    // Solicita ao usuário o número máximo da sequência de Fibonacci a ser exibido
    let mut maximo = String::new();
    println!("Por favor, digite o número máximo da sequência de Fibonacci:");
    io::stdin().read_line(&mut maximo).expect("Falha ao ler a entrada.");
    let maximo: u32 = maximo.trim().parse().expect("Entrada inválida. Por favor, digite um número inteiro positivo.");

    // Chama a função para calcular e exibir a sequência de Fibonacci
    fibonacci(maximo);
}

fn fibonacci(maximo: u32) {
    let mut sequencia: Vec<u32> = vec![0, 1]; // Inicializa a sequência com os dois primeiros números
    let mut i = 2; // Índice para iterar pela sequência

    // Calcula e adiciona os próximos números até atingir o número máximo
    while sequencia[i - 1] + sequencia[i - 2] <= maximo {
        sequencia.push(sequencia[i - 1] + sequencia[i - 2]);
        i += 1;
    }

    // Exibe a sequência de Fibonacci
    println!("Sequência de Fibonacci até {}: {:?}", maximo, sequencia);
}
```

Explicação do código:
1. No início, importamos o módulo `std::io` para permitir a leitura de entrada do usuário.
2. Em seguida, definimos a função `main`, que é a função principal do programa.
3. Na função `main`, imprimimos uma mensagem de boas-vindas e solicitamos ao usuário o número máximo da sequência de Fibonacci a ser exibido.
4. Em seguida, lemos a entrada do usuário, convertemos para o tipo `u32` e armazenamos na variável `maximo`.
5. Chamamos a função `fibonacci` passando o número máximo como argumento.
6. A função `fibonacci` recebe o número máximo como parâmetro e inicializa uma sequência de Fibonacci com os dois primeiros números (0 e 1) armazenados em um vetor `Vec<u32>`.
7. O índice `i` é inicializado como 2, pois já temos os dois primeiros números.
8. Com um loop `while`, calculamos e adicionamos os próximos números da sequência até que a soma dos dois últimos números seja menor ou igual ao número máximo.
9. Por fim, exibimos a sequência de Fibonacci até o número máximo fornecido pelo usuário.