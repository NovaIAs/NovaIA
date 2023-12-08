Claro! Aqui está um código complexo em Kotlin para você:

```kotlin
fun main() {
    // Lista de alunos
    val alunos = mutableListOf<Aluno>()

    // Adicionando alunos à lista
    alunos.add(Aluno("João", "12345678", 18))
    alunos.add(Aluno("Maria", "87654321", 19))
    alunos.add(Aluno("Pedro", "98765432", 17))
    alunos.add(Aluno("Ana", "23456789", 20))
    alunos.add(Aluno("Lucas", "34567890", 21))

    // Imprimindo os dados dos alunos com idade menor que 18 anos
    println("Alunos menores de idade:")
    alunos.filter { it.idade < 18 }.forEach { println(it) }

    // Ordenando a lista de alunos por idade
    alunos.sortBy { it.idade }

    // Imprimindo a lista ordenada
    println("\nAlunos ordenados por idade:")
    alunos.forEach { println(it) }
}

data class Aluno(val nome: String, val matricula: String, val idade: Int) {
    override fun toString(): String {
        return "Nome: $nome\nMatrícula: $matricula\nIdade: $idade\n"
    }
}
```

Nesse código, criamos uma classe `Aluno` com propriedades para nome, matrícula e idade. Em seguida, criamos uma lista de objetos dessa classe e adicionamos cinco alunos à lista.

No exemplo, utilizamos a função `filter` para filtrar e imprimir apenas os alunos menores de idade. Em seguida, usamos a função `sortBy` para ordenar a lista de alunos por idade.

Por fim, imprimimos a lista ordenada de alunos, utilizando a função `forEach` para percorrer cada elemento e imprimir seus dados através da função `toString()` que foi sobrescrita na classe `Aluno`.

Espero que esse código atenda às suas expectativas e demonstre como Kotlin pode ser usado para criar códigos complexos e diferenciados!