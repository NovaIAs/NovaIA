Com certeza! Aqui está um exemplo de código complexo em Kotlin:

```kotlin
fun main() {
    // Classe base para representar um animal
    open class Animal(val nome: String)

    // Classes derivadas de Animal
    class Cachorro(nome: String) : Animal(nome)
    class Gato(nome: String) : Animal(nome)
    class Pato(nome: String) : Animal(nome)

    // Classe para representar uma clínica veterinária
    class ClinicaVeterinaria {
        private val animais: MutableList<Animal> = mutableListOf()

        // Método para adicionar um animal na clínica
        fun adicionarAnimal(animal: Animal) {
            animais.add(animal)
        }

        // Método para exibir todos os animais da clínica
        fun exibirAnimais() {
            println("Lista de animais na clínica veterinária:")
            for (animal in animais) {
                when (animal) {
                    is Cachorro -> println("Cachorro: ${animal.nome}")
                    is Gato -> println("Gato: ${animal.nome}")
                    is Pato -> println("Pato: ${animal.nome}")
                }
            }
        }
    }

    // Criando instâncias de animais
    val cachorro1 = Cachorro("Rex")
    val gato1 = Gato("Bolinha")
    val pato1 = Pato("Donald")

    // Criando instância da clínica veterinária
    val clinica = ClinicaVeterinaria()

    // Adicionando animais na clínica
    clinica.adicionarAnimal(cachorro1)
    clinica.adicionarAnimal(gato1)
    clinica.adicionarAnimal(pato1)

    // Exibindo os animais na clínica
    clinica.exibirAnimais()
}
```

Neste código, temos a criação de uma hierarquia de classes representando animais (Cachorro, Gato e Pato) e uma classe para a clínica veterinária.

A classe `Animal` é uma classe base que contém apenas uma propriedade `nome`. As classes `Cachorro`, `Gato` e `Pato` são derivadas da classe `Animal` e herdam a propriedade `nome`. 

A classe `ClinicaVeterinaria` contém uma lista mutável de animais. Ela possui um método `adicionarAnimal` que permite adicionar um animal à lista e um método `exibirAnimais` que exibe todos os animais presentes na lista. 

No método `main`, criamos instâncias dos animais (um cachorro, um gato e um pato) e uma instância da clínica veterinária. Em seguida, adicionamos os animais na clínica e exibimos a lista de animais presentes na clínica.

Espero que isso seja útil! Caso tenha alguma dúvida ou precise de mais informações, fique à vontade para perguntar.