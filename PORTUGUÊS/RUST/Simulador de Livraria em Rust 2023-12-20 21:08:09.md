Claro! Vou gerar um código complexo em Rust para você. Este código será um simulador de uma livraria, onde será possível adicionar, remover e pesquisar por livros. Vou explicar linha por linha para facilitar o entendimento:

```rust
// Incluindo as bibliotecas necessárias
use std::collections::HashMap;
use std::io::{self, Write};

// Definindo a struct do livro
#[derive(Clone, Debug)]
struct Livro {
    titulo: String,
    autor: String,
    genero: String,
    ano_publicacao: u32,
}

// Definindo a struct da livraria
struct Livraria {
    livros: HashMap<String, Livro>,
}

impl Livraria {
    // Método para adicionar um livro à livraria
    fn adicionar_livro(&mut self, livro: Livro) {
        self.livros.insert(livro.titulo.clone(), livro);
    }

    // Método para remover um livro da livraria
    fn remover_livro(&mut self, titulo: &str) {
        self.livros.remove(titulo);
    }

    // Método para pesquisar por livros na livraria
    fn pesquisar_livro(&self, termo_pesquisa: &str) -> Vec<&Livro> {
        let mut resultados: Vec<&Livro> = Vec::new();
        for livro in self.livros.values() {
            if livro.titulo.contains(termo_pesquisa) || livro.autor.contains(termo_pesquisa) || livro.genero.contains(termo_pesquisa) {
                resultados.push(livro);
            }
        }
        resultados
    }
}

fn main() {
    let mut livraria = Livraria { livros: HashMap::new() };

    loop {
        // Mostrando as opções para o usuário
        println!("Bem-vindo à livraria! O que você deseja fazer?");
        println!("1. Adicionar livro");
        println!("2. Remover livro");
        println!("3. Pesquisar livro");
        println!("4. Sair");

        // Lendo a opção do usuário
        let mut opcao = String::new();
        io::stdin().read_line(&mut opcao).unwrap();

        // Tratando a opção selecionada
        match opcao.trim().parse::<u32>() {
            Ok(1) => {
                // Adicionar livro
                let mut livro = Livro {
                    titulo: String::new(),
                    autor: String::new(),
                    genero: String::new(),
                    ano_publicacao: 0,
                };

                print!("Digite o título do livro: ");
                io::stdout().flush().unwrap();
                io::stdin().read_line(&mut livro.titulo).unwrap();

                print!("Digite o autor do livro: ");
                io::stdout().flush().unwrap();
                io::stdin().read_line(&mut livro.autor).unwrap();

                print!("Digite o gênero do livro: ");
                io::stdout().flush().unwrap();
                io::stdin().read_line(&mut livro.genero).unwrap();

                print!("Digite o ano de publicação do livro: ");
                io::stdout().flush().unwrap();
                let mut ano_publicacao = String::new();
                io::stdin().read_line(&mut ano_publicacao).unwrap();
                livro.ano_publicacao = ano_publicacao.trim().parse().unwrap();

                livraria.adicionar_livro(livro);
                println!("Livro adicionado com sucesso!");
            }
            Ok(2) => {
                // Remover livro
                print!("Digite o título do livro que deseja remover: ");
                io::stdout().flush().unwrap();
                let mut titulo = String::new();
                io::stdin().read_line(&mut titulo).unwrap();
                livraria.remover_livro(&titulo.trim());
                println!("Livro removido com sucesso!");
            }
            Ok(3) => {
                // Pesquisar livro
                print!("Digite um termo de pesquisa (título, autor, gênero): ");
                io::stdout().flush().unwrap();
                let mut termo_pesquisa = String::new();
                io::stdin().read_line(&mut termo_pesquisa).unwrap();

                let resultados = livraria.pesquisar_livro(&termo_pesquisa.trim());

                if resultados.is_empty() {
                    println!("Nenhum livro encontrado.");
                } else {
                    println!("Livros encontrados:");
                    for livro in resultados {
                        println!("{:?}", livro);
                    }
                }
            }
            Ok(4) => {
                // Sair
                println!("Obrigado por utilizar a livraria. Até mais!");
                break;
            }
            _ => {
                println!("Opção inválida. Por favor, tente novamente.");
            }
        }
    }
}
```

Este código em Rust simula uma livraria com as funcionalidades de adicionar, remover e pesquisar livros. A livraria é representada pela struct `Livraria`, que possui um HashMap chamado `livros` para armazenar os livros, onde a chave é o título do livro e o valor é uma instância da struct `Livro`.

A struct `Livro` representa um livro individual, com os campos `titulo`, `autor`, `genero` e `ano_publicacao`.

No método `main`, é criada uma instância da livraria e inicia-se um loop para apresentar um menu de opções ao usuário. O usuário pode adicionar um livro (opção 1), remover um livro (opção 2), pesquisar por livros (opção 3) ou sair (opção 4).

Cada opção é tratada dentro de um bloco `match`. Para adicionar um livro, o programa solicita ao usuário que digite as informações do livro e chama o método `adicionar_livro` da livraria para inserir o livro no HashMap.

Para remover um livro, o programa solicita ao usuário que digite o título do livro a ser removido e chama o método `remover_livro` da livraria para removê-lo do HashMap.

Para pesquisar por livros, o programa solicita ao usuário que digite um termo de pesquisa e chama o método `pesquisar_livro` da livraria, que retorna uma lista de livros que possuem o termo de pesquisa no título, autor ou gênero. Os resultados são exibidos na tela.

Para sair do programa, basta digitar a opção 4.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais detalhes, fique à vontade para perguntar.