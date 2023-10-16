Claro! Aqui está um código complexo em Groovy para você:

```groovy
// Classe para representar uma pessoa
class Pessoa {
    String nome
    int idade
    String endereco

    // Construtor da classe
    Pessoa(String nome, int idade, String endereco) {
        this.nome = nome
        this.idade = idade
        this.endereco = endereco
    }

    // Método para imprimir os dados da pessoa
    void imprimirDados() {
        println("Nome: $nome")
        println("Idade: $idade")
        println("Endereço: $endereco")
    }
}

// Classe principal
class Main {
    static void main(String[] args) {
        // Criando uma lista de pessoas
        List<Pessoa> pessoas = []

        // Adicionando pessoas à lista
        pessoas.add(new Pessoa("João", 25, "Rua A"))
        pessoas.add(new Pessoa("Maria", 30, "Rua B"))
        pessoas.add(new Pessoa("Pedro", 40, "Rua C"))

        // Percorrendo a lista e imprimindo os dados das pessoas
        for (Pessoa pessoa : pessoas) {
            pessoa.imprimirDados()
            println("-----")
        }
    }
}
```

Neste código, criamos uma classe chamada "Pessoa" para representar uma pessoa com nome, idade e endereço. O construtor da classe é responsável por inicializar os atributos da pessoa. 

Além disso, a classe possui um método chamado "imprimirDados" que imprime na tela os dados da pessoa.

Na classe principal "Main", criamos uma lista de pessoas e adicionamos algumas instâncias da classe "Pessoa" a essa lista. Em seguida, percorremos a lista utilizando um loop "for" e chamamos o método "imprimirDados" de cada pessoa, para exibir as informações na tela.

Este código é apenas um exemplo de como criar classes, instanciar objetos e utilizar listas em Groovy. Você pode adaptá-lo conforme suas necessidades e adicionar mais funcionalidades, como métodos de busca, remoção e atualização dos dados das pessoas.