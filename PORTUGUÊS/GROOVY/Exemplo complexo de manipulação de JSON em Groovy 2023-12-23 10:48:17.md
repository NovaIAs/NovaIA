Claro! Aqui está um exemplo de código complexo em Groovy:

```groovy
import groovy.json.JsonOutput
import groovy.json.JsonSlurper

class Pessoa {
    String nome
    int idade
    String endereco
    
    Pessoa(String nome, int idade, String endereco) {
        this.nome = nome
        this.idade = idade
        this.endereco = endereco
    }
    
    void imprimirInformacoes() {
        println("Nome: $nome")
        println("Idade: $idade")
        println("Endereço: $endereco")
    }
}

def pessoasJson = """
{
    "pessoas": [
        {
            "nome": "João",
            "idade": 30,
            "endereco": "Rua A, 123"
        },
        {
            "nome": "Maria",
            "idade": 25,
            "endereco": "Rua B, 456"
        },
        {
            "nome": "Pedro",
            "idade": 35,
            "endereco": "Rua C, 789"
        }
    ]
}
"""

def jsonSlurper = new JsonSlurper()
def pessoas = jsonSlurper.parseText(pessoasJson)["pessoas"]

def listaPessoas = []
pessoas.each { pessoaJson ->
    def pessoa = new Pessoa(pessoaJson.nome, pessoaJson.idade, pessoaJson.endereco)
    listaPessoas.add(pessoa)
}

listaPessoas.each { pessoa ->
    pessoa.imprimirInformacoes()
}

println("Total de pessoas: ${listaPessoas.size()}")
```

Neste exemplo, criamos uma classe `Pessoa` que representa uma pessoa com nome, idade e endereço. Em seguida, utilizamos uma string JSON para representar um array de pessoas com diferentes informações.

Utilizando o `JsonSlurper` do Groovy, fazemos o parse dessa string JSON e obtemos uma lista de objetos JSON. Então, percorremos essa lista e criamos uma instância da classe `Pessoa` para cada objeto JSON, adicionando-as a uma lista `listaPessoas`.

Por fim, percorremos a lista `listaPessoas` e chamamos o método `imprimirInformacoes()` em cada objeto `Pessoa`, exibindo seus detalhes. Também exibimos o total de pessoas na lista.

Esse código é apenas um exemplo complexo e diferenciado em Groovy. Você pode personalizá-lo de acordo com suas necessidades.