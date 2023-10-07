Claro! Aqui está um exemplo de código complexo em Rust, que envolve estruturas de dados avançadas, padrões de projeto e manipulação de arquivos. O código a seguir foi desenvolvido para simular um sistema de gerenciamento de biblioteca.

```rust
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};

struct Livro {
    titulo: String,
    autor: String,
    ano: u32,
}

struct Biblioteca {
    livros: HashMap<String, Livro>,
}

impl Biblioteca {
    fn adicionar_livro(&mut self, titulo: String, autor: String, ano: u32) {
        let livro = Livro {
            titulo: titulo.clone(),
            autor: autor.clone(),
            ano,
        };
        self.livros.insert(titulo, livro);
    }

    fn buscar_livro(&self, titulo: &str) -> Option<&Livro> {
        self.livros.get(titulo)
    }

    fn salvar_para_arquivo(&self, arquivo: &str) -> io::Result<()> {
        let mut file = File::create(arquivo)?;
        for (_, livro) in &self.livros {
            writeln!(file, "{};{};{}", livro.titulo, livro.autor, livro.ano)?;
        }
        Ok(())
    }

    fn carregar_de_arquivo(&mut self, arquivo: &str) -> io::Result<()> {
        let file = File::open(arquivo)?;
        let reader = BufReader::new(file);
        for line in reader.lines() {
            let line = line?;
            let campos: Vec<&str> = line.split(';').collect();
            if campos.len() == 3 {
                let titulo = campos[0].to_string();
                let autor = campos[1].to_string();
                let ano = campos[2].parse::<u32>().unwrap();
                self.adicionar_livro(titulo, autor, ano);
            }
        }
        Ok(())
    }
}

fn main() {
    let mut biblioteca = Biblioteca {
        livros: HashMap::new(),
    };

    biblioteca.adicionar_livro(
        "1984".to_string(),
        "George Orwell".to_string(),
        1949,
    );
    biblioteca.adicionar_livro(
        "A Revolução Dos Bichos".to_string(),
        "George Orwell".to_string(),
        1945,
    );
    biblioteca.adicionar_livro(
        "Dom Quixote".to_string(),
        "Miguel de Cervantes".to_string(),
        1605,
    );

    biblioteca.salvar_para_arquivo("biblioteca.txt").unwrap();

    let mut nova_biblioteca = Biblioteca {
        livros: HashMap::new(),
    };

    nova_biblioteca.carregar_de_arquivo("biblioteca.txt").unwrap();

    if let Some(livro) = nova_biblioteca.buscar_livro("Dom Quixote") {
        println!(
            "Encontrado: {} - {} ({})",
            livro.titulo, livro.autor, livro.ano
        );
    } else {
        println!("Livro não encontrado.");
    }
}
```

Neste código, começamos definindo duas estruturas de dados importantes: `Livro` e `Biblioteca`. A estrutura `Livro` armazena o título, autor e ano de publicação de um livro. A estrutura `Biblioteca` é responsável por gerenciar uma coleção de livros, armazenados em um HashMap. Ela possui métodos para adicionar livros, buscar livros pelo título, salvar a biblioteca em um arquivo e carregar uma biblioteca a partir de um arquivo.

Na função `main()`, criamos uma biblioteca, adicionamos alguns livros e, em seguida, salvamos a biblioteca em um arquivo chamado "biblioteca.txt". Depois, criamos outra biblioteca e carregamos o conteúdo do arquivo "biblioteca.txt" para ela. Finalmente, buscamos pelo livro "Dom Quixote" na nova biblioteca e exibimos as informações caso seja encontrado.

Esse é um exemplo de código complexo em Rust, que utiliza diferentes recursos da linguagem para implementar um sistema de gerenciamento de biblioteca. É importante lembrar que a complexidade do código pode variar dependendo dos requisitos do projeto e das necessidades específicas.