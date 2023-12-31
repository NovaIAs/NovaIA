Claro! Aqui está um código complexo em Rust que atende aos seus requisitos:

```rust
use std::collections::HashMap;
use std::io::{self, BufRead};

fn main() {
    println!("Bem-vindo ao programa de análise de texto!");

    let mut palavras: HashMap<String, u32> = HashMap::new();
    let stdin = io::stdin();

    for line in stdin.lock().lines() {
        let line = line.expect("Erro ao ler linha");
        let words = line.split_whitespace();

        for word in words {
            let count = palavras.entry(word.to_lowercase()).or_insert(0);
            *count += 1;
        }
    }

    let mut palavras_vec: Vec<_> = palavras.into_iter().collect();
    palavras_vec.sort_by(|a, b| b.1.cmp(&a.1));

    println!("Palavras encontradas:");

    for (word, count) in palavras_vec {
        println!("A palavra '{}' apareceu {} vez(es).", word, count);
    }
}
```

Este código lê o texto digitado pelo usuário e conta a frequência de cada palavra. Ele usa um `HashMap` para armazenar as palavras e suas contagens. 

Aqui está uma explicação mais detalhada do código:

1. Importamos os módulos `HashMap` e `io` da biblioteca padrão do Rust.

2. Definimos a função `main` como ponto de entrada do programa.

3. Criamos um `HashMap` chamado `palavras` para armazenar as palavras e suas contagens.

4. Imprimimos uma mensagem de boas-vindas ao usuário.

5. Iniciamos um loop para ler cada linha digitada pelo usuário.

6. Dentro do loop, separamos as palavras da linha usando o método `split_whitespace()`.

7. Para cada palavra encontrada, verificamos se ela já está no `HashMap`. Se estiver, incrementamos a contagem. Caso contrário, inserimos a palavra no `HashMap` com uma contagem inicial de 0 e depois incrementamos.

8. Após o loop, convertemos o `HashMap` em um vetor de tuplas `(String, u32)` chamado `palavras_vec`.

9. Ordenamos o vetor `palavras_vec` em ordem decrescente de contagem.

10. Imprimimos a lista de palavras encontradas, juntamente com suas contagens.

Espero que este código atenda às suas expectativas!