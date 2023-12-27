Claro! Aqui está um exemplo de um código complexo em Groovy:

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
    
    String toString() {
        return "Nome: ${nome}, Idade: ${idade}"
    }
}

// Função para gerar um número aleatório
def gerarNumeroAleatorio(int min, int max) {
    Random random = new Random()
    return random.nextInt(max - min + 1) + min
}

// Lista de pessoas
def pessoas = []

// Gerar 10 pessoas aleatórias
for (int i = 0; i < 10; i++) {
    String nome = "Pessoa ${i+1}"
    int idade = gerarNumeroAleatorio(18, 60)
    Pessoa pessoa = new Pessoa(nome, idade)
    pessoas.add(pessoa)
}

// Imprimir as pessoas
pessoas.each {
    println it
}
```

Neste código, temos uma classe chamada `Pessoa` que representa uma pessoa com nome e idade. Em seguida, temos uma função `gerarNumeroAleatorio` que utiliza a classe `Random` para gerar um número aleatório dentro de um intervalo especificado.

Logo após, temos uma lista chamada `pessoas` que será preenchida com 10 pessoas aleatórias, cada uma com um nome gerado dinamicamente e uma idade entre 18 e 60 anos.

Por fim, utilizamos um loop `each` para percorrer a lista de pessoas e imprimir as informações de cada uma delas.

Espero que este código atenda às suas expectativas!