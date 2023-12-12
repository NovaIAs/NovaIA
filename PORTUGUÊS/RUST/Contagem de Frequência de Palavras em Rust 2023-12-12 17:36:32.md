Claro! Aqui está um exemplo de código complexo em Rust:

```rust
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::collections::HashMap;

#[derive(Debug)]
struct ContagemPalavras {
    palavras: HashMap<String, u32>,
}

impl ContagemPalavras {
    fn novo() -> ContagemPalavras {
        ContagemPalavras {
            palavras: HashMap::new(),
        }
    }

    fn adicionar_palavra(&mut self, palavra: &str) {
        let palavra = palavra.to_lowercase();
        *self.palavras.entry(palavra).or_insert(0) += 1;
    }

    fn salvar_resultados(&self, caminho_arquivo: &str) -> std::io::Result<()> {
        let path = Path::new(caminho_arquivo);
        let mut arquivo = File::create(&path)?;

        for (palavra, contagem) in &self.palavras {
            let linha = format!("{}: {}\n", palavra, contagem);
            arquivo.write_all(linha.as_bytes())?;
        }

        Ok(())
    }
}

fn main() {
    let mut contagem_palavras = ContagemPalavras::novo();

    let texto = "Este é um exemplo de texto, onde estamos contando a frequência das palavras. Este texto é utilizado para testar o código.";

    let palavras: Vec<&str> = texto.split_whitespace().collect();

    for palavra in palavras {
        contagem_palavras.adicionar_palavra(palavra);
    }

    if let Err(erro) = contagem_palavras.salvar_resultados("resultado.txt") {
        eprintln!("Erro ao salvar resultados: {}", erro);
    }
}
```

Este código em Rust realiza a contagem de frequência de palavras em um texto e salva os resultados em um arquivo de texto.

Ele começa definindo uma estrutura chamada `ContagemPalavras` que possui um campo `palavras` do tipo `HashMap<String, u32>`. Essa estrutura possui três métodos: `novo`, que cria uma nova instância; `adicionar_palavra`, que recebe uma palavra como parâmetro e incrementa a contagem dessa palavra no mapa; e `salvar_resultados`, que salva os resultados no arquivo especificado.

No `main`, é criada uma instância de `ContagemPalavras`, o texto a ser analisado é definido e as palavras são separadas utilizando o método `split_whitespace`. Em seguida, cada palavra é adicionada à contagem usando o método `adicionar_palavra`. Por fim, os resultados são salvos em um arquivo chamado "resultado.txt".

Espero que este exemplo atenda às suas expectativas!