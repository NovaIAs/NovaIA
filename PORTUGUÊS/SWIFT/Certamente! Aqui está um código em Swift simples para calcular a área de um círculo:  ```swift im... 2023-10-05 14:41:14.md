Certamente! Aqui está um código complexo em Swift que envolve a criação de uma estrutura de dados personalizada para armazenar informações de alunos em uma escola. Vamos chamá-la de "Escola".

```swift
// Definindo a estrutura Aluno
struct Aluno {
    var nome: String
    var idade: Int
    var notas: [String: Double]
}

// Definindo a estrutura Escola
struct Escola {
    var nome: String
    var endereco: String
    var alunos: [Aluno]
    
    // Método para adicionar um novo aluno à escola
    mutating func adicionarAluno(nome: String, idade: Int, notas: [String: Double]) {
        let aluno = Aluno(nome: nome, idade: idade, notas: notas)
        alunos.append(aluno)
    }
    
    // Método para calcular a média de notas de todos os alunos
    func calcularMediaNotas() -> Double {
        var totalNotas = 0.0
        var quantidadeNotas = 0
        
        for aluno in alunos {
            for (_, nota) in aluno.notas {
                totalNotas += nota
                quantidadeNotas += 1
            }
        }
        
        return totalNotas / Double(quantidadeNotas)
    }
    
    // Método para encontrar o aluno com a maior média de notas
    func encontrarAlunoComMaiorMedia() -> Aluno? {
        var alunoComMaiorMedia: Aluno?
        var maiorMedia = 0.0
        
        for aluno in alunos {
            let media = aluno.notas.values.reduce(0, +) / Double(aluno.notas.count)
            
            if media > maiorMedia {
                maiorMedia = media
                alunoComMaiorMedia = aluno
            }
        }
        
        return alunoComMaiorMedia
    }
}

// Exemplo de uso da estrutura Escola
var minhaEscola = Escola(nome: "Escola ABC", endereco: "Rua da Escola, 123", alunos: [])

minhaEscola.adicionarAluno(nome: "João", idade: 15, notas: ["Matemática": 7.5, "Português": 8.0, "Ciências": 6.5])
minhaEscola.adicionarAluno(nome: "Maria", idade: 14, notas: ["Matemática": 9.0, "Português": 8.5, "Ciências": 7.0])
minhaEscola.adicionarAluno(nome: "Pedro", idade: 16, notas: ["Matemática": 6.0, "Português": 7.5, "Ciências": 8.0])

let mediaNotas = minhaEscola.calcularMediaNotas()
print("A média de notas de todos os alunos é: \(mediaNotas)")

if let alunoComMaiorMedia = minhaEscola.encontrarAlunoComMaiorMedia() {
    print("O aluno com a maior média de notas é: \(alunoComMaiorMedia.nome)")
} else {
    print("Não há alunos na escola.")
}
```

Nesse código, utilizamos as estruturas `Aluno` e `Escola` para armazenar informações dos alunos e criar métodos para adicionar novos alunos, calcular a média de notas de todos os alunos e encontrar o aluno com a maior média de notas.

No exemplo de uso, criamos uma instância da estrutura `Escola`, adicionamos alguns alunos e, em seguida, calculamos a média de notas de todos os alunos e encontramos o aluno com a maior média.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, estou à disposição.