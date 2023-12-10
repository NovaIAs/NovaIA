Claro! Aqui está um exemplo de código complexo em Rust:

```rust
use std::io;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    println!("Bem-vindo ao Gerenciador de Arquivos!");

    loop {
        println!("Escolha uma opção:");
        println!("1. Criar um novo arquivo");
        println!("2. Ler um arquivo existente");
        println!("3. Escrever em um arquivo existente");
        println!("4. Sair");

        let mut opcao = String::new();

        io::stdin().read_line(&mut opcao)
            .expect("Falha ao ler a entrada!");

        let opcao: u32 = match opcao.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Opção inválida! Por favor, tente novamente.");
                continue;
            }
        };

        match opcao {
            1 => criar_arquivo(),
            2 => ler_arquivo(),
            3 => escrever_arquivo(),
            4 => {
                println!("Obrigado por usar o Gerenciador de Arquivos! Até logo!");
                break;
            }
            _ => {
                println!("Opção inválida! Por favor, tente novamente.");
                continue;
            }
        }
    }
}

fn criar_arquivo() {
    println!("Digite o nome do arquivo:");

    let mut nome_arquivo = String::new();

    io::stdin().read_line(&mut nome_arquivo)
        .expect("Falha ao ler a entrada!");

    let nome_arquivo = nome_arquivo.trim();

    let arquivo = File::create(nome_arquivo)
        .expect("Falha ao criar o arquivo!");

    println!("Arquivo '{}' criado com sucesso!", nome_arquivo);
}

fn ler_arquivo() {
    println!("Digite o nome do arquivo:");

    let mut nome_arquivo = String::new();

    io::stdin().read_line(&mut nome_arquivo)
        .expect("Falha ao ler a entrada!");

    let nome_arquivo = nome_arquivo.trim();

    let mut arquivo = File::open(nome_arquivo)
        .expect("Falha ao abrir o arquivo!");

    let mut conteudo = String::new();

    arquivo.read_to_string(&mut conteudo)
        .expect("Falha ao ler o arquivo!");

    println!("Conteúdo do arquivo '{}':\n{}", nome_arquivo, conteudo);
}

fn escrever_arquivo() {
    println!("Digite o nome do arquivo:");

    let mut nome_arquivo = String::new();

    io::stdin().read_line(&mut nome_arquivo)
        .expect("Falha ao ler a entrada!");

    let nome_arquivo = nome_arquivo.trim();

    let mut arquivo = OpenOptions::new()
        .append(true)
        .open(nome_arquivo)
        .expect("Falha ao abrir o arquivo!");

    println!("Digite o texto que deseja escrever:");

    let mut texto = String::new();

    io::stdin().read_line(&mut texto)
        .expect("Falha ao ler a entrada!");

    arquivo.write_all(texto.as_bytes())
        .expect("Falha ao escrever no arquivo!");

    println!("Texto adicionado com sucesso ao arquivo '{}'!", nome_arquivo);
}
```

Este código é um gerenciador básico de arquivos em Rust. Ele permite ao usuário criar um novo arquivo, ler o conteúdo de um arquivo existente e escrever no final de um arquivo existente. O programa é executado em um loop até que o usuário escolha a opção de sair.

O código utiliza a biblioteca padrão `std::io` para lidar com a entrada e saída de dados, e `std::fs::File` para operações de arquivo. O programa começa exibindo um menu de opções e lê a opção escolhida pelo usuário. Dependendo da opção selecionada, o programa chama a função correspondente para executar a ação desejada.

Por exemplo, a função `criar_arquivo` solicita ao usuário o nome do arquivo a ser criado e usa `File::create` para criar o arquivo. A função `ler_arquivo` solicita ao usuário o nome do arquivo a ser lido e usa `File::open` para abrir o arquivo. Em seguida, lê o conteúdo do arquivo e o exibe na saída. A função `escrever_arquivo` solicita ao usuário o nome do arquivo e o texto a ser adicionado, e usa `OpenOptions::new().append(true)` para abrir o arquivo em modo de anexação e `File::write_all` para escrever o texto no arquivo.

Espero que este exemplo seja útil para você!