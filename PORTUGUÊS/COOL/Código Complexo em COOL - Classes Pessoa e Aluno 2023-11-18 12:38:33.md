Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language), explicando cada parte do código para facilitar o entendimento:

```
class Pessoa {

    atributo nome : String <- "João"
    atributo idade : Int <- 25

    metodo imprimirDetalhes() : Object {
        out_string("Nome: ")
        out_string(nome)
        out_string("\n")
        out_string("Idade: ")
        out_int(idade)
        out_string("\n")
    }

    metodo calcularIdade() : Object {
        se idade < 18 então
            out_string("Você é menor de idade!\n")
        senão
            out_string("Você é maior de idade!\n")
        fim_se
    }
}

class Aluno herda_de Pessoa {

    atributo numeroMatricula : Int <- 123456
    atributo curso : String <- "Engenharia"

    metodo imprimirDetalhes() : Object {
        out_string("Nome: ")
        out_string(nome)
        out_string("\n")
        out_string("Idade: ")
        out_int(idade)
        out_string("\n")
        out_string("Número de Matrícula: ")
        out_int(numeroMatricula)
        out_string("\n")
        out_string("Curso: ")
        out_string(curso)
        out_string("\n")
    }

    metodo calcularIdade() : Object {
        se idade < 18 então
            out_string("O aluno é menor de idade!\n")
        senão
            out_string("O aluno é maior de idade!\n")
        fim_se
    }
}

metodo principal() : Object {
    var pessoa : Pessoa <- new Pessoa
    var aluno : Aluno <- new Aluno

    pessoa.imprimirDetalhes()
    aluno.imprimirDetalhes()

    pessoa.calcularIdade()
    aluno.calcularIdade()

    retorno 0
}
```

Neste código, criamos duas classes em COOL: "Pessoa" e "Aluno". A classe "Aluno" herda da classe "Pessoa". 

A classe "Pessoa" possui dois atributos: "nome" (do tipo String) e "idade" (do tipo Int) que são inicializados com valores padrão. Ela também possui dois métodos: "imprimirDetalhes()" e "calcularIdade()". O método "imprimirDetalhes()" imprime na saída padrão o nome e a idade da pessoa. Já o método "calcularIdade()" verifica se a idade é menor ou maior que 18 e imprime uma mensagem correspondente.

A classe "Aluno" herda da classe "Pessoa" e possui dois atributos adicionais: "numeroMatricula" (do tipo Int) e "curso" (do tipo String). Ela também sobrescreve os métodos "imprimirDetalhes()" e "calcularIdade()" para imprimir informações específicas do aluno.

Por fim, no método "principal()", criamos uma instância de cada classe e invocamos os métodos "imprimirDetalhes()" e "calcularIdade()" para cada objeto. O programa retorna 0 no final.

Este código é apenas um exemplo complexo para fins de demonstração. Se você tiver algum requisito específico, posso gerar um código de acordo com suas necessidades.