Claro! Aqui está um exemplo de um código complexo em Groovy que realiza uma tarefa complexa de manipulação de dados em uma lista:

```groovy
import groovy.transform.ToString

@ToString(includeNames = true)
class Pessoa {
    String nome
    int idade

    Pessoa(String nome, int idade) {
        this.nome = nome
        this.idade = idade
    }
}

def listaPessoas = [
    new Pessoa("João", 25),
    new Pessoa("Maria", 30),
    new Pessoa("Pedro", 35),
    new Pessoa("Ana", 40)
]

def filtrarPorIdadeMaiorQue(lista, idade) {
    lista.findAll { pessoa -> pessoa.idade > idade }
}

def mapearNomes(lista) {
    lista.collect { pessoa -> pessoa.nome }
}

def ordenarPorIdade(lista) {
    lista.sort { pessoa1, pessoa2 -> pessoa1.idade <=> pessoa2.idade }
}

def imprimirLista(lista) {
    println "Lista de pessoas:"
    lista.each { pessoa -> println pessoa }
}

println "Aplicando filtro de idade..."
def pessoasFiltradas = filtrarPorIdadeMaiorQue(listaPessoas, 30)
imprimirLista(pessoasFiltradas)

println "\nMapeando nomes..."
def nomes = mapearNomes(listaPessoas)
println "Nomes: $nomes"

println "\nOrdenando por idade..."
ordenarPorIdade(listaPessoas)
imprimirLista(listaPessoas)
```

Neste código, temos uma classe `Pessoa` definida com os atributos `nome` e `idade`. Em seguida, criamos uma lista chamada `listaPessoas` com alguns objetos da classe `Pessoa`.

Em seguida, temos três métodos principais:

1. `filtrarPorIdadeMaiorQue`: recebe uma lista e uma idade como parâmetros e retorna uma nova lista contendo apenas as pessoas cuja idade é maior que a idade fornecida.

2. `mapearNomes`: recebe uma lista e mapeia apenas os nomes das pessoas, retornando uma nova lista com os nomes.

3. `ordenarPorIdade`: recebe uma lista e ordena as pessoas com base em suas idades.

Por fim, temos um método `imprimirLista` que imprime os elementos da lista fornecida.

No programa principal, chamamos os métodos para filtrar as pessoas com idade maior que 30, mapear os nomes e ordenar por idade. Em seguida, imprimimos as listas resultantes.