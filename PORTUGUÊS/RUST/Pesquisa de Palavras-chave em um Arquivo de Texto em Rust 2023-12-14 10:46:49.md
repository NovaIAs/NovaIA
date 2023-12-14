Claro! Aqui está um código complexo em Rust que realiza uma pesquisa de palavras-chave em um arquivo de texto:

```rust
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    // Obtém o primeiro argumento da linha de comando como o nome do arquivo
    let args: Vec<String> = env::args().collect();
    let file_name = match args.get(1) {
        Some(name) => name,
        None => {
            eprintln!("É necessário fornecer o nome do arquivo como argumento!");
            return;
        }
    };

    // Abre o arquivo especificado
    let file_path = Path::new(file_name);
    let file = match File::open(&file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Erro ao abrir o arquivo {}: {}", file_path.display(), err);
            return;
        }
    };

    // Lê o conteúdo do arquivo linha por linha
    let reader = io::BufReader::new(file);
    for (line_number, line) in reader.lines().enumerate() {
        match line {
            Ok(line) => {
                let words: Vec<&str> = line.split_whitespace().collect();
                for (word_number, word) in words.iter().enumerate() {
                    // Lista de palavras-chave a serem pesquisadas
                    let keywords = vec!["Keyword1", "Keyword2", "Keyword3"];

                    // Verifica se a palavra atual é uma das palavras-chave
                    if keywords.contains(&word) {
                        println!("Palavra-chave encontrada na linha {} e na posição {}: {}", line_number + 1, word_number + 1, word);
                    }
                }
            }
            Err(err) => {
                eprintln!("Erro ao ler a linha {}: {}", line_number + 1, err);
                continue;
            }
        }
    }
}
```

Este código em Rust lê um arquivo de texto fornecido como argumento na linha de comando e realiza uma pesquisa de palavras-chave. O usuário precisa fornecer o nome do arquivo como argumento ao executar o programa.

O código começa verificando se o nome do arquivo foi fornecido como argumento na linha de comando. Caso contrário, exibe uma mensagem de erro e encerra o programa.

Em seguida, o código tenta abrir o arquivo especificado. Se ocorrer um erro ao abrir o arquivo, exibe uma mensagem de erro e encerra o programa.

Depois de abrir o arquivo com sucesso, o código lê seu conteúdo linha por linha. Cada linha é dividida em palavras usando o método `split_whitespace()`, que retorna um iterador de palavras. Em seguida, cada palavra é verificada em relação a uma lista de palavras-chave predefinidas.

Se uma palavra-chave for encontrada em uma linha, o código exibirá uma mensagem indicando o número da linha, a posição da palavra e a própria palavra-chave.

Espero que este código atenda às suas expectativas!