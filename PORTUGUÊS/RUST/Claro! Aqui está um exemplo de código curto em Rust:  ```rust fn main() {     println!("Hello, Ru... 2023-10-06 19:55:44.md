Claro! Aqui está um exemplo de código complexo em Rust, que utiliza algumas funcionalidades avançadas da linguagem:

```rust
// Esta é uma função recursiva que calcula o n-ésimo número de Fibonacci
fn fibonacci(n: u32) -> u32 {
    if n <= 1 {
        return n;
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
}

fn main() {
    // Aqui estamos criando um vetor para armazenar os números de Fibonacci
    let mut fibonacci_nums: Vec<u32> = Vec::new();
    
    // Pedimos ao usuário para inserir a quantidade de números de Fibonacci que ele deseja calcular
    println!("Insira a quantidade de números de Fibonacci que você deseja calcular:");
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).expect("Falha ao ler a entrada do usuário");
    let num_fibonacci: u32 = input.trim().parse().expect("Entrada inválida");
    
    // Calculamos e armazenamos os números de Fibonacci no vetor
    for n in 0..num_fibonacci {
        fibonacci_nums.push(fibonacci(n));
    }
    
    // Imprimimos os números de Fibonacci calculados
    println!("Os {} primeiros números de Fibonacci são:", num_fibonacci);
    for num in fibonacci_nums {
        print!("{} ", num);
    }
}
```

Neste código, temos uma função recursiva chamada `fibonacci` que calcula o n-ésimo número de Fibonacci. Ela recebe como parâmetro um número inteiro `n` e retorna o valor correspondente na sequência de Fibonacci.

No `main`, criamos um vetor chamado `fibonacci_nums` para armazenar os números de Fibonacci calculados. Em seguida, solicitamos ao usuário que insira a quantidade de números de Fibonacci que ele deseja calcular.

Depois de ler a entrada do usuário, utilizamos um loop `for` para calcular e armazenar os números de Fibonacci no vetor `fibonacci_nums`, chamando a função `fibonacci` para cada valor de `n` de 0 até `num_fibonacci - 1`.

Por fim, imprimimos os números de Fibonacci calculados utilizando outro loop `for` e exibimos o resultado na tela.

Espero que esse código complexo em Rust possa te ajudar!