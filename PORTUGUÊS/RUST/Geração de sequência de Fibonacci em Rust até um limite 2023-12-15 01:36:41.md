Claro! Aqui está um código complexo em Rust que realiza uma tarefa interessante: gerar uma sequência de Fibonacci até um determinado número limite.

```rust
fn main() {
    // Função para gerar a sequência de Fibonacci
    fn fibonacci_sequence(limit: u32) -> Vec<u32> {
        let mut sequence = Vec::new();

        let (mut a, mut b) = (0, 1);
        while a <= limit {
            sequence.push(a);
            let temp = a + b;
            a = b;
            b = temp;
        }

        sequence
    }

    // Função para imprimir a sequência de Fibonacci
    fn print_fibonacci_sequence(sequence: &[u32]) {
        for num in sequence {
            print!("{} ", num);
        }
        println!();
    }

    // Solicita o número limite ao usuário
    println!("Digite o número limite para a sequência de Fibonacci: ");
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Falha ao ler a entrada");
    let limit: u32 = input.trim().parse().expect("Digite um número válido");

    // Gera e imprime a sequência de Fibonacci até o limite fornecido
    let fibonacci_sequence = fibonacci_sequence(limit);
    print_fibonacci_sequence(&fibonacci_sequence);
}
```

Neste código, temos uma função `fibonacci_sequence` que recebe um limite como argumento e retorna um vetor contendo a sequência de Fibonacci até esse limite. A sequência é gerada usando um loop `while` e os números são armazenados no vetor `sequence`.

Em seguida, temos a função `print_fibonacci_sequence` que recebe uma referência imutável para o vetor `sequence` e imprime os números na sequência.

No `main`, solicitamos ao usuário que digite o número limite desejado. Em seguida, lemos a entrada do usuário, convertemos para o tipo `u32` e armazenamos na variável `limit`.

Por fim, chamamos a função `fibonacci_sequence` passando o limite fornecido pelo usuário, armazenamos o resultado na variável `fibonacci_sequence` e chamamos a função `print_fibonacci_sequence` para imprimir a sequência gerada.

Espero que esse código atenda às suas expectativas.