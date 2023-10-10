Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que utiliza conceitos avançados de orientação a objetos e estruturas de dados.

```
class Aluno {
    // Atributos
    atributo nome : String <- ""
    atributo idade : Int <- 0
    atributo notas : List<Float> <- nil

    // Método construtor
    inicialização novo(nome : String, idade : Int) : SELF_TYPE {
        {
            self.nome <- nome;
            self.idade <- idade;
            self.notas <- List<Float>.novo()
            início
                para i de 0 até 4 faça
                    notafinal : Float <- gerarNota()
                    self.notas.append(notafinal)
                fimpara
            fim
            self
        }
    }

    // Método para gerar nota aleatória
    método privado gerarNota() : Float {
        {
            resultado : Float <- 0.0
            // Gera uma nota entre 0.0 e 10.0
            resultado <- Float.random(10.0)
            resultado
        }
    }

    // Método para calcular a média das notas
    método público médiaNotas() : Float {
        {
            soma : Float <- 0.0
            quantidade : Int <- self.notas.tamanho()
            para i de 0 até quantidade-1 faça
                soma <- soma + self.notas[i]
            fimpara
            soma / quantidade
        }
    }
}

// Classe principal
classe Main {
    método principal() : Object {
        {
            aluno : Aluno <- Aluno.novo("João", 20)
            // Exibe as notas do aluno
            para i de 0 até aluno.notas.tamanho()-1 faça
                out_string("Nota " + i + ": " + aluno.notas[i].string())
            fimpara
            // Calcula e exibe a média das notas
            out_string("Média das notas: " + aluno.médiaNotas().string())
        }
    }
}
```

Neste código, é criada uma classe `Aluno` que possui os atributos `nome`, `idade` e `notas`. O método construtor `novo` recebe o nome e a idade do aluno e inicializa o objeto com esses valores. Além disso, ele utiliza um loop para gerar 5 notas aleatórias e armazená-las na lista de notas.

A classe `Aluno` também possui o método `médiaNotas`, que calcula a média das notas armazenadas na lista `notas`.

A classe `Main` possui o método `principal`, que cria um objeto `Aluno` com o nome "João" e a idade 20. Em seguida, ele itera sobre a lista de notas do aluno e exibe cada uma delas. Por fim, calcula e exibe a média das notas.

Espero que esse código complexo em COOL seja útil para você!