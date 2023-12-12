Claro! Aqui está um exemplo de código em COOL (Classroom Object-Oriented Language) que simula um sistema de gerenciamento de alunos em uma escola:

```
class Aluno {

    atributo nome: String <- ""
    atributo idade: Int <- 0
    atributo notas: Array of Float <- []

    metodo init(n: String, i: Int, nt: Array of Float) : Aluno {
        nome <- n
        idade <- i
        notas <- nt
        retorne self
    }

    metodo calcularMedia() : Float {
        var soma: Float <- 0.0
        var quantidadeNotas: Int <- notas.tamanho()

        se quantidadeNotas = 0 entao
            retorne 0.0
        senao
            para cada nota em notas faca
                soma <- soma + nota
            fim para

            retorne soma / quantidadeNotas
        fim se
    }

    metodo imprimirInformacoes() : Nada {
        imprima("Nome: ", nome)
        imprima("Idade: ", idade)
        imprima("Notas: ", notas)
        imprima("Média: ", calcularMedia())
    }
}

class Escola {

    atributo alunos: Array of Aluno <- []

    metodo adicionarAluno(a: Aluno) : Nada {
        alunos.inserirNoFim(a)
    }

    metodo removerAluno(a: Aluno) : Nada {
        alunos.remover(a)
    }

    metodo buscarAlunoPorNome(nome: String) : Aluno {
        para cada aluno em alunos faca
            se aluno.nome = nome entao
                retorne aluno
            fim se
        fim para

        retorne nil
    }

    metodo imprimirAlunos() : Nada {
        para cada aluno em alunos faca
            aluno.imprimirInformacoes()
            imprima("-------")
        fim para
    }
}

// Exemplo de uso

var escola: Escola <- Escola.new()

var aluno1: Aluno <- Aluno.new().init("João", 16, [8.5, 9.0, 7.5])
var aluno2: Aluno <- Aluno.new().init("Maria", 17, [7.0, 6.5, 8.0])
var aluno3: Aluno <- Aluno.new().init("Pedro", 15, [9.5, 8.0, 9.0])

escola.adicionarAluno(aluno1)
escola.adicionarAluno(aluno2)
escola.adicionarAluno(aluno3)

escola.imprimirAlunos()

var alunoBuscado: Aluno <- escola.buscarAlunoPorNome("Maria")

se alunoBuscado != nil entao
    alunoBuscado.imprimirInformacoes()
senao
    imprima("Aluno não encontrado")
fim se
```

Neste código, temos duas classes principais: `Aluno` e `Escola`. A classe `Aluno` possui atributos como nome, idade e notas (um array de notas). Ela também possui métodos para inicializar os atributos, calcular a média das notas e imprimir as informações do aluno. A classe `Escola` é responsável por gerenciar os alunos, armazenando-os em um array e fornecendo métodos para adicionar, remover e buscar alunos pelo nome.

No exemplo de uso no final do código, criamos uma instância da classe `Escola` e alguns alunos. Em seguida, adicionamos os alunos à escola e imprimimos as informações de todos os alunos. Depois, fazemos uma busca por um aluno específico pelo nome e imprimimos suas informações, se encontrado.

Espero que este código atenda às suas expectativas de complexidade e diferenciação!