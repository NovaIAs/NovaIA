Claro! Aqui está um código complexo em Rust para você:

```rust
use std::io::{self, BufRead};

struct Pessoa {
    nome: String,
    idade: u32,
}

impl Pessoa {
    fn new(nome: String, idade: u32) -> Self {
        Pessoa { nome, idade }
    }

    fn dizer_oi(&self) {
        println!("Olá, meu nome é {} e tenho {} anos.", self.nome, self.idade);
    }
}

fn main() {
    let stdin = io::stdin();
    let mut pessoas: Vec<Pessoa> = Vec::new();

    loop {
        println!("O que você deseja fazer?");
        println!("1. Adicionar uma pessoa");
        println!("2. Exibir todas as pessoas");
        println!("3. Sair");

        let mut choice = String::new();
        stdin.lock().read_line(&mut choice).expect("Falha ao ler a entrada");

        match choice.trim().parse::<u32>() {
            Ok(1) => {
                println!("Digite o nome da pessoa:");
                let mut nome = String::new();
                stdin.lock().read_line(&mut nome).expect("Falha ao ler a entrada");

                println!("Digite a idade da pessoa:");
                let mut idade = String::new();
                stdin.lock().read_line(&mut idade).expect("Falha ao ler a entrada");

                let pessoa = Pessoa::new(nome.trim().to_string(), idade.trim().parse().expect("Falha ao converter idade"));

                pessoas.push(pessoa);
            }
            Ok(2) => {
                for pessoa in pessoas.iter() {
                    pessoa.dizer_oi();
                }
            }
            Ok(3) => break,
            _ => println!("Escolha inválida!"),
        }
    }
}
```

Este código em Rust cria uma estrutura chamada `Pessoa` que possui dois campos: `nome` (do tipo `String`) e `idade` (do tipo `u32`). Em seguida, é implementado um método `new` para criar uma nova instância da estrutura `Pessoa`.

No `main`, é criado um loop onde o usuário pode escolher entre adicionar uma pessoa, exibir todas as pessoas ou sair do programa. A escolha do usuário é lida a partir da entrada padrão (`stdin`).

Se o usuário escolher adicionar uma pessoa, o programa solicita o nome e a idade da pessoa. Em seguida, uma nova instância de `Pessoa` é criada com base nos valores digitados e adicionada ao vetor `pessoas`.

Se o usuário escolher exibir todas as pessoas, o programa itera sobre o vetor `pessoas` e chama o método `dizer_oi` para cada pessoa, exibindo na tela o nome e a idade.

Se o usuário escolher sair do programa, o loop é interrompido e o programa é encerrado.

Espero que esse código atenda às suas expectativas! Se você tiver mais alguma solicitação, fique à vontade para perguntar.