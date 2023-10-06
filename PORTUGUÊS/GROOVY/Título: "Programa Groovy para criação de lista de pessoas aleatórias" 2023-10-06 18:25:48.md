Claro! Aqui está um código complexo em Groovy:

```groovy
import java.util.Random

class Pessoa {
    String nome
    int idade
    
    Pessoa(String nome, int idade) {
        this.nome = nome
        this.idade = idade
    }
    
    void apresentar() {
        println("Olá, meu nome é ${nome} e eu tenho ${idade} anos.")
    }
}

class Programa {
    static void main(String[] args) {
        def listaPessoas = []
        def random = new Random()
        
        // Criando uma lista de 10 pessoas com nomes aleatórios e idades entre 18 e 30 anos
        for (int i = 0; i < 10; i++) {
            def nome = gerarNomeAleatorio()
            def idade = random.nextInt(13) + 18
            def pessoa = new Pessoa(nome, idade)
            listaPessoas.add(pessoa)
        }
        
        // Apresentando todas as pessoas da lista
        for (Pessoa pessoa : listaPessoas) {
            pessoa.apresentar()
        }
    }
    
    static String gerarNomeAleatorio() {
        def nomes = ["Alice", "Bob", "Carol", "David", "Eve", "Frank", "Grace", "Hank"]
        def random = new Random()
        def indice = random.nextInt(nomes.size())
        return nomes[indice]
    }
}
```

Neste código em Groovy, criamos duas classes: `Pessoa` e `Programa`. A classe `Pessoa` possui dois atributos: `nome` e `idade`. Ela também possui um construtor para inicializar esses atributos e um método `apresentar()` que imprime uma mensagem com o nome e idade da pessoa.

A classe `Programa` é onde tudo acontece. Primeiro, criamos uma lista vazia chamada `listaPessoas` e um objeto da classe `Random` para gerar números aleatórios. Em seguida, utilizamos um loop `for` para criar 10 pessoas com nomes aleatórios (usando o método `gerarNomeAleatorio()`) e idades entre 18 e 30 anos. Cada pessoa é adicionada à lista.

Depois, utilizamos outro loop `for` para percorrer a lista de pessoas e chamar o método `apresentar()` de cada uma delas, mostrando suas informações na tela.

O método `gerarNomeAleatorio()` seleciona um nome aleatório da lista `nomes` utilizando o objeto `Random` e retorna esse nome.

Espero que este código complexo em Groovy atenda às suas expectativas!