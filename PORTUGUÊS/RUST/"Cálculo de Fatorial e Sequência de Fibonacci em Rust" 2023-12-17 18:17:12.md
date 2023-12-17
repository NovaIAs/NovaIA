Claro! Aqui está um exemplo de código complexo em Rust que envolve manipulação de dados e cálculos matemáticos:

```rust
// Função para calcular o fatorial de um número
fn fatorial(n: u32) -> u32 {
    if n == 0 {
        return 1;
    }
    return n * fatorial(n - 1);
}

// Função para calcular o número de Fibonacci
fn fibonacci(n: u32) -> u32 {
    if n <= 1 {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// Função principal
fn main() {
    // Solicita ao usuário um número para calcular o fatorial
    println!("Digite um número para calcular o fatorial:");
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).expect("Falha ao ler entrada");
    let numero: u32 = input.trim().parse().expect("Entrada inválida");

    // Calcula e exibe o fatorial do número digitado
    let fatorial_numero = fatorial(numero);
    println!("O fatorial de {} é: {}", numero, fatorial_numero);

    // Solicita ao usuário um número de Fibonacci a ser calculado
    println!("Digite um número para calcular a sequência de Fibonacci:");
    input = String::new();
    std::io::stdin().read_line(&mut input).expect("Falha ao ler entrada");
    let numero: u32 = input.trim().parse().expect("Entrada inválida");

    // Calcula e exibe a sequência de Fibonacci até o número digitado
    println!("A sequência de Fibonacci até {} é:", numero);
    for i in 0..=numero {
        println!("{}", fibonacci(i));
    }
}
```

Nesse código, temos três partes principais:

1. A função `fatorial` é responsável por calcular o fatorial de um número usando recursão. Ela recebe um parâmetro `n` do tipo `u32` (número inteiro não negativo) e retorna o fatorial desse número.

2. A função `fibonacci` é responsável por calcular o número de Fibonacci para um dado valor `n`. Também utiliza recursão para fazer o cálculo e retorna o número de Fibonacci correspondente.

3. A função `main` é a função principal do programa. Ela solicita ao usuário um número para calcular o fatorial e depois solicita outro número para calcular a sequência de Fibonacci. Em seguida, chama as funções `fatorial` e `fibonacci` para realizar os cálculos e exibe os resultados na tela.

Espero que este exemplo atenda às suas expectativas! Se você tiver mais alguma solicitação, é só pedir.