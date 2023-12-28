Certamente! Aqui está um código complexo em Swift que envolve a criação de uma estrutura de dados personalizada, a implementação de um algoritmo de ordenação e a manipulação de diferentes tipos de dados. Vou dividir o código em partes para facilitar a explicação:

```swift
// Definindo a estrutura de dados personalizada
struct Pessoa {
    var nome: String
    var idade: Int
}

// Implementando o algoritmo de ordenação usando o Bubble Sort
func bubbleSort<T: Comparable>(_ array: inout [T]) {
    let n = array.count
    
    for i in 0..<n {
        for j in 0..<n-i-1 {
            if array[j] > array[j+1] {
                array.swapAt(j, j+1)
            }
        }
    }
}

// Criando uma lista de pessoas
var pessoas = [Pessoa(nome: "João", idade: 25),
               Pessoa(nome: "Maria", idade: 30),
               Pessoa(nome: "Pedro", idade: 20)]

// Ordenando as pessoas por idade usando o algoritmo de ordenação
bubbleSort(&pessoas) { $0.idade < $1.idade }

// Exibindo as pessoas ordenadas por idade
for pessoa in pessoas {
    print("Nome: \(pessoa.nome) - Idade: \(pessoa.idade)")
}
```

Explicação:
1. Definimos a estrutura de dados `Pessoa` com as propriedades `nome` e `idade`.
2. Implementamos a função `bubbleSort` que recebe um array de elementos comparáveis (usamos um tipo genérico `T: Comparable`) e usa o algoritmo de Bubble Sort para ordenar o array em ordem crescente.
3. Criamos uma lista de pessoas, com cada pessoa representada por uma instância da estrutura `Pessoa`.
4. Chamamos a função `bubbleSort` passando o array de pessoas e uma closure que define o critério de ordenação (nesse caso, a idade da pessoa).
5. Por fim, percorremos o array ordenado e exibimos o nome e a idade de cada pessoa.

Esse código demonstra a criação de uma estrutura de dados personalizada, a implementação de um algoritmo de ordenação e a manipulação de elementos usando closures em Swift.