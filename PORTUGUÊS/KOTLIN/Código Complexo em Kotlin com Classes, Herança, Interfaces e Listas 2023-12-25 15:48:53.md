Claro! Aqui está um código complexo em Kotlin que envolve o uso de classes, herança, interfaces e manipulação de listas:

```kotlin
// Definindo uma classe abstrata chamada Animal
abstract class Animal(val nome: String) {
    abstract fun fazerBarulho()
}

// Definindo uma interface chamada Voador
interface Voador {
    fun voar()
}

// Definindo uma classe chamada Ave que herda de Animal e implementa a interface Voador
class Ave(nome: String) : Animal(nome), Voador {
    override fun fazerBarulho() {
        println("$nome está fazendo barulho de ave")
    }

    override fun voar() {
        println("$nome está voando")
    }
}

// Definindo uma classe chamada Mamifero que herda de Animal
class Mamifero(nome: String) : Animal(nome) {
    override fun fazerBarulho() {
        println("$nome está fazendo barulho de mamífero")
    }
}

// Função principal
fun main() {
    // Criando uma lista de animais
    val animais = listOf(
        Ave("Papagaio"),
        Mamifero("Leão"),
        Ave("Pombo"),
        Mamifero("Gato")
    )

    // Iterando sobre a lista de animais
    for (animal in animais) {
        animal.fazerBarulho()

        if (animal is Voador) {
            animal.voar()
        }
        
        println()
    }
}
```

Neste código, começamos definindo uma classe abstrata chamada `Animal`, que possui um atributo `nome` e um método abstrato `fazerBarulho()`. Em seguida, definimos uma interface chamada `Voador`, que possui um método `voar()`. 

Depois, definimos uma classe chamada `Ave` que herda de `Animal` e implementa a interface `Voador`. Essa classe sobrescreve o método `fazerBarulho()` para exibir uma mensagem específica para aves e o método `voar()` para exibir uma mensagem informando que o animal está voando.

Também definimos uma classe chamada `Mamifero` que herda de `Animal` e sobrescreve o método `fazerBarulho()` para exibir uma mensagem específica para mamíferos.

Na função `main()`, criamos uma lista de animais que contém instâncias de `Ave` e `Mamifero`. Em seguida, iteramos sobre essa lista, chamando o método `fazerBarulho()` de cada animal e, se o animal for uma instância de `Voador`, chamamos o método `voar()` também.

Espero que este exemplo complexo atenda às suas expectativas! Lembre-se de que você pode personalizar e modificar o código conforme necessário.