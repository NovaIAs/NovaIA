Claro! Aqui está um exemplo de código complexo em Rust que pode ser útil para você entender melhor o funcionamento de structs, traits, e manipulação de dados:

```rust
// Definindo uma struct chamada `Pessoa` com dois campos: nome e idade
struct Pessoa {
    nome: String,
    idade: u32,
}

// Definindo um trait chamado `Imprimivel` que possui um método `imprimir`
trait Imprimivel {
    fn imprimir(&self);
}

// Implementando o trait `Imprimivel` para a struct `Pessoa`
impl Imprimivel for Pessoa {
    fn imprimir(&self) {
        println!("Nome: {}, Idade: {}", self.nome, self.idade);
    }
}

// Criando uma função que recebe um vetor de pessoas e imprime cada uma
fn imprimir_pessoas<T: Imprimivel>(pessoas: Vec<T>) {
    for pessoa in pessoas {
        pessoa.imprimir();
    }
}

fn main() {
    // Criando algumas instâncias da struct `Pessoa`
    let pessoa1 = Pessoa {
        nome: String::from("João"),
        idade: 25,
    };
    
    let pessoa2 = Pessoa {
        nome: String::from("Maria"),
        idade: 30,
    };
    
    let pessoa3 = Pessoa {
        nome: String::from("Carlos"),
        idade: 35,
    };

    // Criando um vetor com as instâncias de `Pessoa`
    let pessoas = vec![pessoa1, pessoa2, pessoa3];

    // Chamando a função `imprimir_pessoas` passando o vetor de pessoas
    imprimir_pessoas(pessoas);
}
```

Neste exemplo, começamos definindo uma struct chamada `Pessoa` com dois campos: `nome` (do tipo `String`) e `idade` (do tipo `u32`). Em seguida, criamos um trait chamado `Imprimivel` com um método `imprimir`. 

Implementamos o trait `Imprimivel` para a struct `Pessoa`, onde definimos a implementação do método `imprimir` para exibir o nome e a idade da pessoa.

Em seguida, criamos uma função chamada `imprimir_pessoas` que recebe um vetor de qualquer tipo `T` que implemente o trait `Imprimivel`. Essa função itera sobre as pessoas do vetor e chama o método `imprimir` para cada uma.

No `main`, criamos algumas instâncias da struct `Pessoa` e as adicionamos a um vetor. Por fim, chamamos a função `imprimir_pessoas`, passando o vetor de pessoas como argumento.

Este código é apenas um exemplo complexo em Rust, mas pode ser adaptado e personalizado de acordo com as suas necessidades.