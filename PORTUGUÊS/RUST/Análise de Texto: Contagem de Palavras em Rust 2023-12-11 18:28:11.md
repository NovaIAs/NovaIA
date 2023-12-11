Claro! Vou criar um código em Rust bastante complexo que envolve a manipulação de dados, estruturas de controle e funções. Ele será um programa que realiza a análise de texto para contar a ocorrência de cada palavra em um arquivo. Vou explicar cada parte do código para facilitar o entendimento.

```rust
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Obter o nome do arquivo a ser analisado a partir dos argumentos da linha de comando
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    // Abrir o arquivo e ler o conteúdo para uma string
    let mut file = File::open(filename).expect("Não foi possível abrir o arquivo.");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Não foi possível ler o arquivo.");

    // Chamar a função `contar_palavras` para realizar a análise e obter o resultado
    let resultado = contar_palavras(&contents);

    // Imprimir o resultado
    for (palavra, ocorrencias) in resultado {
        println!("{}: {}", palavra, ocorrencias);
    }
}

fn contar_palavras(texto: &str) -> HashMap<String, u32> {
    let mut ocorrencias: HashMap<String, u32> = HashMap::new();

    // Dividir o texto em palavras separadas por espaços em branco
    let palavras: Vec<&str> = texto.split_whitespace().collect();

    // Iterar sobre todas as palavras e contar suas ocorrências
    for palavra in palavras {
        // Remover possíveis caracteres especiais (pontuação, etc.) e transformar em minúsculas
        let palavra_limpa = palavra
            .chars()
            .filter(|c| c.is_alphabetic())
            .map(|c| c.to_lowercase())
            .collect::<String>();

        // Incrementar o contador da palavra
        *ocorrencias.entry(palavra_limpa).or_insert(0) += 1;
    }

    ocorrencias
}
```

Neste código, utilizamos algumas bibliotecas padrão do Rust, como `std::collections::HashMap` para armazenar as palavras e suas ocorrências, `std::env` para obter argumentos da linha de comando e `std::fs::File` para abrir e ler um arquivo.

No `main`, obtemos o nome do arquivo a ser analisado a partir dos argumentos da linha de comando. Em seguida, abrimos o arquivo, lemos seu conteúdo para uma string e chamamos a função `contar_palavras` para realizar a análise.

A função `contar_palavras` recebe uma referência para um texto e retorna um `HashMap` contendo as palavras e suas ocorrências. Primeiro, dividimos o texto em palavras separadas por espaços em branco utilizando o método `split_whitespace`. Em seguida, iteramos sobre todas as palavras, limpando-as de possíveis caracteres especiais, transformando-as em minúsculas e incrementando o contador de ocorrências.

Por fim, no `main`, imprimimos o resultado obtido, mostrando cada palavra e a quantidade de vezes que ela ocorre no texto.