Certamente! Aqui está um exemplo de um código complexo em Swift que envolve uma estrutura de dados, algoritmo de ordenação e manipulação de arquivos:

```swift
import Foundation

// Definindo a estrutura de dados Pessoa
struct Pessoa {
    var nome: String
    var idade: Int
}

// Função para ordenar uma lista de pessoas por idade
func ordenarPessoasPorIdade(pessoas: [Pessoa]) -> [Pessoa] {
    var pessoasOrdenadas = pessoas
    
    for i in 0..<pessoasOrdenadas.count {
        for j in 0..<pessoasOrdenadas.count - 1 {
            if pessoasOrdenadas[j].idade > pessoasOrdenadas[j + 1].idade {
                let temp = pessoasOrdenadas[j]
                pessoasOrdenadas[j] = pessoasOrdenadas[j + 1]
                pessoasOrdenadas[j + 1] = temp
            }
        }
    }
    
    return pessoasOrdenadas
}

// Função para salvar a lista de pessoas em um arquivo
func salvarPessoasEmArquivo(pessoas: [Pessoa], nomeArquivo: String) {
    let fileURL = URL(fileURLWithPath: nomeArquivo)
    
    var texto = ""
    for pessoa in pessoas {
        texto += "\(pessoa.nome),\(pessoa.idade)\n"
    }
    
    do {
        try texto.write(to: fileURL, atomically: true, encoding: .utf8)
        print("Pessoas salvas com sucesso!")
    } catch {
        print("Erro ao salvar pessoas: \(error.localizedDescription)")
    }
}

// Código principal
let pessoas = [
    Pessoa(nome: "João", idade: 25),
    Pessoa(nome: "Maria", idade: 30),
    Pessoa(nome: "Pedro", idade: 20)
]

let pessoasOrdenadas = ordenarPessoasPorIdade(pessoas: pessoas)
salvarPessoasEmArquivo(pessoas: pessoasOrdenadas, nomeArquivo: "pessoas.txt")
```

Neste código, definimos uma estrutura de dados chamada `Pessoa`, que possui propriedades para o nome e idade de uma pessoa. Em seguida, temos a função `ordenarPessoasPorIdade`, que recebe uma lista de pessoas e utiliza o algoritmo de ordenação bubble sort para ordená-las em ordem crescente de idade.

Também temos a função `salvarPessoasEmArquivo`, que recebe a lista ordenada de pessoas e um nome de arquivo como parâmetros. Essa função cria um texto formatado com o nome e idade de cada pessoa, separados por vírgula, e salva esse texto em um arquivo com o nome especificado.

No código principal, criamos uma lista de pessoas com nomes e idades fictícios. Em seguida, chamamos a função `ordenarPessoasPorIdade` para obter a lista ordenada e, por fim, chamamos a função `salvarPessoasEmArquivo` para salvar essa lista no arquivo "pessoas.txt".

Espero que este exemplo atenda às suas expectativas de complexidade e diferenciação!