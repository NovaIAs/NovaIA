Claro! Aqui está um código complexo em Rust que envolve a manipulação de strings e o uso de funções recursivas para cálculos matemáticos:

```rust
fn main() {
    // Função para calcular o fatorial de um número
    fn fatorial(n: u64) -> u64 {
        if n == 0 {
            1
        } else {
            n * fatorial(n - 1)
        }
    }

    // Função para inverter uma string recursivamente
    fn inverter_string(s: &str) -> String {
        if s.is_empty() {
            String::new()
        } else {
            let mut chars = s.chars();
            let char = chars.next().unwrap();
            inverter_string(chars.as_str()).chars().chain(std::iter::once(char)).collect()
        }
    }

    // Função para verificar se uma palavra é um palíndromo
    fn eh_palindromo(palavra: &str) -> bool {
        let palavra_invertida = inverter_string(palavra);
        palavra == palavra_invertida
    }

    // Função para calcular a soma dos fatoriais dos dígitos de um número
    fn soma_fatoriais_digitos(numero: u64) -> u64 {
        let digits = numero.to_string();
        let mut soma = 0;

        for c in digits.chars() {
            let digito = c.to_digit(10).unwrap() as u64;
            soma += fatorial(digito);
        }

        soma
    }

    // Função principal
    fn executar() {
        let numero = 12345;
        let soma = soma_fatoriais_digitos(numero);

        println!("O número {} tem soma dos fatoriais dos dígitos igual a {}", numero, soma);

        let palavra = "arara";
        let palindromo = eh_palindromo(palavra);

        if palindromo {
            println!("A palavra {} é um palíndromo", palavra);
        } else {
            println!("A palavra {} não é um palíndromo", palavra);
        }
    }

    executar();
}
```

Neste código, temos uma função para calcular o fatorial de um número, uma função para inverter uma string recursivamente, uma função para verificar se uma palavra é um palíndromo e uma função para calcular a soma dos fatoriais dos dígitos de um número. 

A função `executar` é a função principal que chama as demais funções e imprime os resultados na tela. No exemplo, estamos usando o número 12345 para calcular a soma dos fatoriais dos dígitos e a palavra "arara" para verificar se é um palíndromo.

Espero que este código atenda às suas necessidades!