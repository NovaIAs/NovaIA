Claro! Aqui está um código em Groovy que realiza uma tarefa complexa e único:

```groovy
import java.util.Random

// Classe para representar uma Pessoa
class Pessoa {
    String nome
    int idade
    
    Pessoa(String nome, int idade) {
        this.nome = nome
        this.idade = idade
    }
}

// Função para gerar um número aleatório
int gerarNumeroAleatorio(int min, int max) {
    return new Random().nextInt((max - min) + 1) + min
}

// Função para verificar se uma pessoa é maior de idade
boolean verificarMaiorIdade(Pessoa pessoa) {
    return pessoa.idade >= 18
}

// Função principal
void principal() {
    // Criar uma lista de pessoas
    List<Pessoa> pessoas = [
        new Pessoa("João", gerarNumeroAleatorio(10, 50)),
        new Pessoa("Maria", gerarNumeroAleatorio(10, 50)),
        new Pessoa("Pedro", gerarNumeroAleatorio(10, 50)),
        new Pessoa("Ana", gerarNumeroAleatorio(10, 50))
    ]
    
    // Filtrar as pessoas maiores de idade
    List<Pessoa> pessoasMaioresIdade = pessoas.findAll { pessoa ->
        verificarMaiorIdade(pessoa)
    }
    
    // Imprimir as pessoas maiores de idade
    println "Pessoas maiores de idade:"
    pessoasMaioresIdade.each { pessoa ->
        println "Nome: ${pessoa.nome}, Idade: ${pessoa.idade}"
    }
    
    // Calcular a soma das idades das pessoas menores de idade
    int somaIdadesMenoresIdade = pessoas.inject(0) { soma, pessoa ->
        if (!verificarMaiorIdade(pessoa)) {
            soma += pessoa.idade
        }
        soma
    }
    
    // Imprimir a soma das idades das pessoas menores de idade
    println "Soma das idades das pessoas menores de idade: $somaIdadesMenoresIdade"
}

// Chamada da função principal
principal()
```

Nesse código em Groovy, criamos uma classe `Pessoa` que possui um nome e idade. Em seguida, definimos uma função `gerarNumeroAleatorio` que gera um número aleatório dentro de um intervalo especificado.

Na função principal `principal()`, criamos uma lista de pessoas com nomes e idades aleatórios. Em seguida, utilizamos a função `findAll` para filtrar as pessoas maiores de idade e as imprimimos na tela.

Depois, utilizamos a função `inject` para calcular a soma das idades das pessoas menores de idade e também imprimimos o resultado.

Por fim, chamamos a função principal para executar todo o código.

Esse código é apenas um exemplo complexo e único, mas pode ser adaptado ou melhorado de acordo com a necessidade.