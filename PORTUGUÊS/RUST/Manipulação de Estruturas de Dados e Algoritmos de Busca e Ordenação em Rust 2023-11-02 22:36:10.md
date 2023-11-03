Claro! Aqui está um exemplo de um código complexo em Rust que envolve a manipulação de estruturas de dados e a implementação de algoritmos de busca e ordenação:

```rust
// Definindo uma estrutura para representar uma pessoa
struct Pessoa {
    nome: String,
    idade: u8,
}

impl Pessoa {
    // Método para criar uma nova pessoa
    fn nova(nome: &str, idade: u8) -> Self {
        Pessoa {
            nome: nome.to_string(),
            idade,
        }
    }
}

fn main() {
    // Criando um vetor de pessoas
    let mut pessoas = vec![
        Pessoa::nova("João", 25),
        Pessoa::nova("Maria", 30),
        Pessoa::nova("Pedro", 18),
        Pessoa::nova("Ana", 35),
        Pessoa::nova("Lucas", 22),
    ];

    // Ordenando as pessoas pelo nome em ordem alfabética
    pessoas.sort_by(|a, b| a.nome.cmp(&b.nome));

    // Imprimindo as pessoas ordenadas
    println!("Pessoas ordenadas pelo nome:");
    for pessoa in &pessoas {
        println!("Nome: {}, Idade: {}", pessoa.nome, pessoa.idade);
    }

    // Buscando uma pessoa pelo nome
    let nome_busca = "Maria";
    let pessoa_encontrada = pessoas
        .iter()
        .find(|&p| p.nome == nome_busca);

    match pessoa_encontrada {
        Some(p) => println!(
            "Pessoa encontrada: {} - Idade: {}",
            p.nome, p.idade
        ),
        None => println!("Pessoa não encontrada."),
    }
}
```

Neste código, temos uma estrutura `Pessoa` que representa uma pessoa com dois campos: `nome` e `idade`. A estrutura também possui um método `nova` que permite a criação de uma nova instância de `Pessoa`.

No `main`, criamos um vetor de pessoas e o ordenamos pelo nome usando o método `sort_by` e a função de comparação `cmp`. Em seguida, imprimimos as pessoas ordenadas.

Em seguida, fazemos uma busca por uma pessoa específica pelo nome usando o método `find` e a função de comparação `p.nome == nome_busca`. Se a pessoa for encontrada, imprimimos suas informações, caso contrário, exibimos uma mensagem informando que a pessoa não foi encontrada.