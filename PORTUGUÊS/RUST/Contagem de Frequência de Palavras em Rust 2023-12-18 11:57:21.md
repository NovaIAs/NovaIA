Claro! Aqui está um código complexo em Rust que envolve a criação de uma estrutura de dados personalizada, ordenação, busca e manipulação de arquivos. O código irá ler um arquivo de texto, contar a frequência de cada palavra e, em seguida, imprimir as palavras mais frequentes em ordem decrescente de ocorrência.

```rust
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

struct WordFrequency {
    word: String,
    frequency: u32,
}

fn main() {
    // Lê o arquivo de texto
    let mut file = File::open("texto.txt").expect("Falha ao abrir o arquivo");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Falha ao ler o arquivo");

    // Remove caracteres especiais e converte todas as palavras para minúsculas
    let words = contents
        .to_lowercase()
        .split(|c: char| !c.is_alphabetic())
        .filter(|word| !word.is_empty())
        .collect::<Vec<&str>>();

    // Conta a frequência de cada palavra
    let mut word_frequency: HashMap<&str, u32> = HashMap::new();
    for word in words {
        *word_frequency.entry(word).or_insert(0) += 1;
    }

    // Converte o HashMap em uma lista de WordFrequency
    let mut word_frequency_list: Vec<WordFrequency> = word_frequency
        .iter()
        .map(|(&word, &frequency)| WordFrequency { word: word.to_string(), frequency })
        .collect();

    // Ordena a lista por frequência em ordem decrescente
    word_frequency_list.sort_by(|a, b| b.frequency.cmp(&a.frequency));

    // Imprime as palavras mais frequentes
    for word_frequency in word_frequency_list {
        println!("Palavra: {}, Frequência: {}", word_frequency.word, word_frequency.frequency);
    }
}
```

Neste código, primeiro definimos uma estrutura de dados chamada `WordFrequency` que armazena uma palavra e sua frequência. Em seguida, abrimos um arquivo de texto chamado "texto.txt" e lemos seu conteúdo para uma string. Depois, removemos caracteres especiais e convertemos todas as palavras para minúsculas.

Em seguida, usamos um HashMap chamado `word_frequency` para contar a frequência de cada palavra no texto. Iteramos sobre as palavras, incrementando o valor correspondente no HashMap para cada palavra encontrada.

Em seguida, convertemos o HashMap em uma lista de `WordFrequency` para facilitar a ordenação. Usamos a função `sort_by` para ordenar a lista em ordem decrescente de frequência.

Por fim, imprimimos as palavras mais frequentes junto com sua frequência usando um loop `for`.