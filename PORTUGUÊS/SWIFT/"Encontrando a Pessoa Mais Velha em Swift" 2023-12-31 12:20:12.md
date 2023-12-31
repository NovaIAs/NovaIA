Claro! Aqui está um código complexo em Swift que envolve manipulação de dados, estruturas de controle e funções:

```swift
// Definindo uma estrutura chamada "Pessoa" para representar uma pessoa com nome e idade
struct Pessoa {
    var nome: String
    var idade: Int
}

// Criando um array de objetos "Pessoa"
var pessoas: [Pessoa] = [
    Pessoa(nome: "João", idade: 25),
    Pessoa(nome: "Maria", idade: 30),
    Pessoa(nome: "Pedro", idade: 40),
    Pessoa(nome: "Ana", idade: 35)
]

// Função para encontrar a pessoa mais velha no array
func encontrarPessoaMaisVelha(pessoas: [Pessoa]) -> Pessoa? {
    guard !pessoas.isEmpty else {
        return nil
    }
    
    var pessoaMaisVelha = pessoas[0]
    
    for pessoa in pessoas {
        if pessoa.idade > pessoaMaisVelha.idade {
            pessoaMaisVelha = pessoa
        }
    }
    
    return pessoaMaisVelha
}

// Chamando a função para encontrar a pessoa mais velha
if let pessoaMaisVelha = encontrarPessoaMaisVelha(pessoas: pessoas) {
    print("A pessoa mais velha é \(pessoaMaisVelha.nome) com \(pessoaMaisVelha.idade) anos.")
} else {
    print("Não existem pessoas no array.")
}
```

Neste código, começamos definindo uma estrutura chamada "Pessoa" que possui duas propriedades: nome e idade. Em seguida, criamos um array de objetos "Pessoa" chamado "pessoas" com algumas instâncias.

Depois, definimos a função "encontrarPessoaMaisVelha" que recebe um array de objetos "Pessoa" como parâmetro e retorna uma instância de "Pessoa" representando a pessoa mais velha. Essa função usa um loop "for" para percorrer todas as pessoas do array e compara as idades para encontrar a mais velha.

Por fim, chamamos a função "encontrarPessoaMaisVelha" passando o array "pessoas" como argumento. Se a função retornar uma pessoa, imprimimos o nome e a idade da pessoa mais velha. Caso contrário, exibimos uma mensagem informando que não existem pessoas no array.

Espero que este código atenda às suas necessidades!