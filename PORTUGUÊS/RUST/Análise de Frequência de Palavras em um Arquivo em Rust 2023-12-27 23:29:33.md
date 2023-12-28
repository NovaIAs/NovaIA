Claro! Aqui está um código complexo em Rust, que realiza uma análise de texto para contar e exibir a frequência de ocorrência de cada palavra em um arquivo:

```rust
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    // Recebe o nome do arquivo como argumento de linha de comando
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    // Abre o arquivo
    let file = File::open(filename).expect("Não foi possível abrir o arquivo.");

    // Cria um leitor para o arquivo
    let reader = BufReader::new(file);

    // Cria um HashMap para armazenar a frequência de cada palavra
    let mut word_freq: HashMap<String, u32> = HashMap::new();

    // Itera através de cada linha do arquivo
    for line in reader.lines() {
        if let Ok(line) = line {
            // Separa as palavras da linha por espaços em branco
            let words: Vec<&str> = line.split_whitespace().collect();

            // Itera através de cada palavra
            for word in words {
                // Remove caracteres especiais e converte para minúsculas
                let cleaned_word = word
                    .chars()
                    .filter(|c| c.is_alphabetic())
                    .map(|c| c.to_lowercase().to_string())
                    .collect::<String>();

                // Incrementa a frequência da palavra no HashMap
                *word_freq.entry(cleaned_word).or_insert(0) += 1;
            }
        }
    }

    // Ordena as palavras por frequência
    let mut sorted_words: Vec<(&String, &u32)> = word_freq.iter().collect();
    sorted_words.sort_by(|a, b| b.1.cmp(a.1));

    // Exibe as palavras e suas frequências
    for (word, freq) in sorted_words {
        println!("{}: {}", word, freq);
    }
}
```

Este código recebe o nome de um arquivo como argumento de linha de comando e realiza a análise de texto para contar e exibir a frequência de ocorrência de cada palavra no arquivo.

Primeiro, o código abre o arquivo especificado e cria um leitor para lê-lo. Em seguida, um HashMap chamado `word_freq` é criado para armazenar a frequência de cada palavra. 

O código itera através de cada linha do arquivo e divide a linha em palavras separadas por espaços em branco. Em seguida, cada palavra é limpa, removendo caracteres especiais e convertendo para minúsculas. A frequência da palavra é incrementada no HashMap usando o método `entry`, que retorna uma referência mutável para a entrada correspondente no HashMap. Se a palavra ainda não estiver presente no HashMap, uma nova entrada é inserida com uma frequência inicial de 0.

Após percorrer todas as linhas do arquivo, o HashMap é convertido em um vetor de tuplas `(word, freq)` e ordenado por ordem decrescente de frequência.

Por fim, as palavras e suas frequências são exibidas na tela.

Espero que este código complexo em Rust atenda às suas expectativas!