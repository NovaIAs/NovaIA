Claro! Aqui está um código complexo em Rust que implementa um algoritmo de criptografia de substituição polialfabética conhecido como Cifra de Vigenère. Essa cifra utiliza uma série de tabelas de alfabeto deslocadas para criptografar e descriptografar mensagens. 

```rust
fn cifra_vigenere(texto: &str, chave: &str) -> String {
    let mut resultado = String::new();
    let alfabeto = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let tamanho_alfabeto = alfabeto.chars().count();

    for (i, c) in texto.chars().enumerate() {
        if c.is_ascii_alphabetic() {
            let letra_texto = c.to_ascii_uppercase();
            let letra_chave = chave.chars().nth(i % chave.len()).unwrap().to_ascii_uppercase();
            let indice_texto = alfabeto.chars().position(|x| x == letra_texto).unwrap();
            let indice_chave = alfabeto.chars().position(|x| x == letra_chave).unwrap();

            let indice_cifrado = (indice_texto + indice_chave) % tamanho_alfabeto;
            let letra_cifrada = alfabeto.chars().nth(indice_cifrado).unwrap();
            resultado.push(letra_cifrada);
        } else {
            resultado.push(c);
        }
    }

    resultado
}

fn decifra_vigenere(texto_cifrado: &str, chave: &str) -> String {
    let mut resultado = String::new();
    let alfabeto = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let tamanho_alfabeto = alfabeto.chars().count();

    for (i, c) in texto_cifrado.chars().enumerate() {
        if c.is_ascii_alphabetic() {
            let letra_cifrada = c.to_ascii_uppercase();
            let letra_chave = chave.chars().nth(i % chave.len()).unwrap().to_ascii_uppercase();
            let indice_cifrada = alfabeto.chars().position(|x| x == letra_cifrada).unwrap();
            let indice_chave = alfabeto.chars().position(|x| x == letra_chave).unwrap();

            let indice_decifrada = (indice_cifrada + tamanho_alfabeto - indice_chave) % tamanho_alfabeto;
            let letra_decifrada = alfabeto.chars().nth(indice_decifrada).unwrap();
            resultado.push(letra_decifrada);
        } else {
            resultado.push(c);
        }
    }

    resultado
}

fn main() {
    let mensagem = "HELLO WORLD";
    let chave = "RUST";

    let mensagem_cifrada = cifra_vigenere(mensagem, chave);
    println!("Mensagem Cifrada: {}", mensagem_cifrada);

    let mensagem_decifrada = decifra_vigenere(&mensagem_cifrada, chave);
    println!("Mensagem Decifrada: {}", mensagem_decifrada);
}
```

Neste código, temos duas funções principais: `cifra_vigenere` e `decifra_vigenere`. A função `cifra_vigenere` recebe um texto e uma chave, e retorna a mensagem cifrada utilizando a cifra de Vigenère. A função `decifra_vigenere` recebe um texto cifrado e a chave correspondente, e retorna a mensagem original decifrada.

A implementação utiliza o alfabeto em letras maiúsculas e considera apenas caracteres alfabéticos. A lógica da cifra de Vigenère é aplicada para cada letra do texto, utilizando o deslocamento determinado pela letra correspondente da chave. A função `cifra_vigenere` realiza o deslocamento para cifrar o texto, enquanto a função `decifra_vigenere` realiza o deslocamento inverso para decifrar o texto cifrado.

No exemplo principal, o código cifra a mensagem "HELLO WORLD" utilizando a chave "RUST" e imprime a mensagem cifrada. Em seguida, decifra a mensagem cifrada utilizando a mesma chave e imprime a mensagem decifrada.