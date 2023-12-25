Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de notas para uma escola fictícia:

```
class Aluno {

    nome: String <- nil,
    notas: ListaNota <- new ListaNota

    init(nome: String) : Object {
        self.nome <- nome
        self.notas <- new ListaNota
        self
    }

    adicionarNota(nota: Float) : SELF_TYPE {
        notas.adicionarNota(nota)
        self
    }

    calcularMedia() : Float {
        notas.calcularMedia()
    }

    imprimirNotas() : Object {
        notas.imprimirNotas()
    }

}

class ListaNota {

    notas: Array of Float <- new Array of Float(0),
    tamanho: Int <- 0

    init() : Object {
        self.notas <- new Array of Float(10)
        self.tamanho <- 0
        self
    }

    adicionarNota(nota: Float) : Object {
        notas[tamanho] <- nota
        tamanho <- tamanho + 1
        self
    }

    calcularMedia() : Float {
        soma: Float <- 0.0
        media: Float <- 0.0

        if tamanho = 0 then
            return media
        else
            i: Int <- 0
            while i < tamanho loop
                soma <- soma + notas[i]
                i <- i + 1
            pool

            media <- soma / tamanho
            return media
        fi
    }

    imprimirNotas() : Object {
        i: Int <- 0

        while i < tamanho loop
            out_string("Nota ")
            out_int(i+1)
            out_string(": ")
            out_float(notas[i])
            out_string("\n")
            i <- i + 1
        pool
    }

}

class Main {

    main() : Object {
        aluno: Aluno <- new Aluno("João")
        aluno.adicionarNota(8.5)
        aluno.adicionarNota(7.0)
        aluno.adicionarNota(9.5)

        out_string("Notas do aluno ")
        out_string(aluno.nome)
        out_string(":\n")
        aluno.imprimirNotas()

        out_string("Média: ")
        out_float(aluno.calcularMedia())
        out_string("\n")

        self
    }
}
```

Neste código, temos três classes principais: `Aluno`, `ListaNota` e `Main`. A classe `Aluno` representa um aluno da escola e possui um atributo `nome` para armazenar o nome do aluno e um atributo `notas` do tipo `ListaNota` para armazenar as notas do aluno.

A classe `ListaNota` é responsável por armazenar e calcular as notas dos alunos. Ela possui um atributo `notas` do tipo `Array of Float` para armazenar as notas e um atributo `tamanho` para controlar o tamanho do array.

A classe `Main` é onde o programa principal é executado. No método `main()`, criamos um objeto `Aluno` chamado `aluno` com o nome "João" e adicionamos algumas notas usando o método `adicionarNota()`. Em seguida, chamamos os métodos `imprimirNotas()` e `calcularMedia()` para exibir as notas e a média do aluno.

Espero que este código atenda às suas expectativas!