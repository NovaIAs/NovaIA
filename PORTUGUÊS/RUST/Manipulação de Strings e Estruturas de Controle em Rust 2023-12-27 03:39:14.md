Claro! Aqui está um código complexo em Rust que envolve manipulação de strings e estruturas de controle:

```rust
fn main() {
    // Vetor de palavras
    let palavras = vec!["Olá", "mundo", "eu", "sou", "um", "código", "complexo", "em", "Rust"];

    // Função para inverter uma string
    fn inverter_string(s: &str) -> String {
        s.chars().rev().collect()
    }

    // Função para contar o número de vogais em uma string
    fn contar_vogais(s: &str) -> u32 {
        s.chars().filter(|c| "aeiou".contains(*c)).count() as u32
    }

    // Vetor para armazenar as palavras invertidas
    let mut palavras_invertidas: Vec<String> = Vec::new();

    // Laço para inverter cada palavra e adicioná-la ao vetor
    for palavra in palavras.iter() {
        palavras_invertidas.push(inverter_string(palavra));
    }

    // Variável para armazenar o número total de vogais
    let mut total_vogais = 0;

    // Laço para contar o número de vogais em cada palavra invertida e somar ao total
    for palavra in palavras_invertidas.iter() {
        total_vogais += contar_vogais(palavra);
    }

    // Impressão das palavras invertidas e do total de vogais
    println!("Palavras invertidas:");
    for palavra in palavras_invertidas.iter() {
        println!("{}", palavra);
    }

    println!("Total de vogais: {}", total_vogais);
}
```

Neste código em Rust, temos um vetor de palavras inicial e duas funções: `inverter_string` para inverter uma string e `contar_vogais` para contar o número de vogais em uma string.

Em seguida, criamos um vetor vazio `palavras_invertidas` para armazenar as palavras invertidas. Usamos um laço `for` para percorrer cada palavra do vetor `palavras`, chamamos a função `inverter_string` para inverter a palavra e adicionamos a palavra invertida ao vetor `palavras_invertidas`.

Após isso, temos uma variável `total_vogais` para armazenar o número total de vogais. Utilizamos outro laço `for` para percorrer cada palavra invertida do vetor `palavras_invertidas`, chamamos a função `contar_vogais` para contar o número de vogais da palavra invertida e somamos esse valor à variável `total_vogais`.

Por fim, imprimimos as palavras invertidas e o total de vogais.

Espero que este código atenda às suas expectativas!