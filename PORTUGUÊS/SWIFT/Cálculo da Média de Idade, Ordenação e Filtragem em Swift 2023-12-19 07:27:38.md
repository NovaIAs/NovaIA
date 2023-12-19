Claro! Aqui está um exemplo de um código complexo em Swift:

```swift
import Foundation

// Definindo uma estrutura para representar uma pessoa
struct Pessoa {
    var nome: String
    var idade: Int
}

// Criando uma função para calcular a média de idade das pessoas
func calcularMediaIdade(pessoas: [Pessoa]) -> Double {
    let somaIdades = pessoas.reduce(0) { $0 + $1.idade }
    return Double(somaIdades) / Double(pessoas.count)
}

// Criando uma função para ordenar as pessoas por idade
func ordenarPorIdade(pessoas: [Pessoa]) -> [Pessoa] {
    return pessoas.sorted { $0.idade < $1.idade }
}

// Criando uma função para filtrar as pessoas com idade acima de um limite
func filtrarPorIdade(pessoas: [Pessoa], limite: Int) -> [Pessoa] {
    return pessoas.filter { $0.idade > limite }
}

// Criando um array de pessoas
let pessoas = [
    Pessoa(nome: "João", idade: 25),
    Pessoa(nome: "Maria", idade: 30),
    Pessoa(nome: "Pedro", idade: 20),
    Pessoa(nome: "Ana", idade: 35),
    Pessoa(nome: "Carlos", idade: 40)
]

// Calculando a média de idade das pessoas
let mediaIdade = calcularMediaIdade(pessoas: pessoas)
print("A média de idade das pessoas é: \(mediaIdade)")

// Ordenando as pessoas por idade
let pessoasOrdenadas = ordenarPorIdade(pessoas: pessoas)
print("Pessoas ordenadas por idade:")
for pessoa in pessoasOrdenadas {
    print("\(pessoa.nome) - \(pessoa.idade) anos")
}

// Filtrando as pessoas com idade acima de 30 anos
let pessoasFiltradas = filtrarPorIdade(pessoas: pessoas, limite: 30)
print("Pessoas com idade acima de 30 anos:")
for pessoa in pessoasFiltradas {
    print("\(pessoa.nome) - \(pessoa.idade) anos")
}
```

Neste exemplo, temos a definição de uma estrutura chamada `Pessoa`, que representa uma pessoa com nome e idade. Em seguida, temos três funções: `calcularMediaIdade`, que recebe um array de pessoas e retorna a média de idade delas; `ordenarPorIdade`, que recebe um array de pessoas e retorna um novo array ordenado por idade; e `filtrarPorIdade`, que recebe um array de pessoas e um limite de idade, e retorna um novo array contendo apenas as pessoas com idade acima desse limite.

Em seguida, criamos um array de pessoas e realizamos algumas operações com ele. Primeiro, calculamos a média de idade das pessoas e a imprimimos. Depois, ordenamos as pessoas por idade e as imprimimos. Por fim, filtramos as pessoas com idade acima de 30 anos e as imprimimos.

Espero que este exemplo atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais informações, estou aqui para ajudar.