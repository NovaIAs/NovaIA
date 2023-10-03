Claro! Aqui está um exemplo de um código complexo em Swift que envolve manipulação de dados e implementação de um algoritmo de ordenação.

```swift
import Foundation

// Definindo uma estrutura para representar uma pessoa
struct Pessoa {
    var nome: String
    var idade: Int
}

// Função para ordenar uma lista de pessoas por idade
func ordenarPessoasPorIdade(pessoas: [Pessoa]) -> [Pessoa] {
    var pessoasOrdenadas = pessoas
    
    for i in 0..<pessoasOrdenadas.count {
        var minIndex = i
        
        for j in i+1..<pessoasOrdenadas.count {
            if pessoasOrdenadas[j].idade < pessoasOrdenadas[minIndex].idade {
                minIndex = j
            }
        }
        
        if minIndex != i {
            pessoasOrdenadas.swapAt(i, minIndex)
        }
    }
    
    return pessoasOrdenadas
}

// Função principal
func main() {
    // Criando uma lista de pessoas
    let pessoas = [
        Pessoa(nome: "João", idade: 27),
        Pessoa(nome: "Maria", idade: 35),
        Pessoa(nome: "Pedro", idade: 19),
        Pessoa(nome: "Ana", idade: 42),
        Pessoa(nome: "Lucas", idade: 24)
    ]
    
    // Ordenando as pessoas por idade
    let pessoasOrdenadas = ordenarPessoasPorIdade(pessoas: pessoas)
    
    // Imprimindo o resultado
    for pessoa in pessoasOrdenadas {
        print("\(pessoa.nome) - \(pessoa.idade) anos")
    }
}

// Chamando a função principal
main()
```

Neste código, estamos criando uma estrutura chamada `Pessoa` para representar uma pessoa com nome e idade. Em seguida, temos a função `ordenarPessoasPorIdade`, que recebe uma lista de pessoas e utiliza o algoritmo de ordenação conhecido como "Selection Sort" para ordenar as pessoas por idade.

Na função principal `main`, criamos uma lista de pessoas com nomes e idades diferentes. Em seguida, chamamos a função `ordenarPessoasPorIdade` passando a lista de pessoas como parâmetro. Por fim, imprimimos o resultado ordenado na tela.

Espero que este código atenda às suas expectativas! Se tiver mais alguma solicitação, estou à disposição para ajudar.