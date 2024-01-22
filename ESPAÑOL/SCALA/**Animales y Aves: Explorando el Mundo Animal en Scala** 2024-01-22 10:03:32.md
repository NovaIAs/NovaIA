```scala
// Clase Animal - Crea una clase de rasgos comunes de los animales
abstract class Animal(nombre: String, edad: Int) {
    val nombreAnimal: String = nombre
    val edadAnimal: Int = edad

    def hacerRuido(): String
}

// Clase Perro - Hereda de la clase Animal y define comportamientos y atributos específicos de los perros
class Perro(nombre: String, edad: Int, raza: String) extends Animal(nombre, edad) {
    val razaPerro: String = raza

    // Método sobreescrito para definir el ruido específico de un perro
    override def hacerRuido(): String = "Guau, Guau!"
}

// Clase Gato - Hereda de la clase Animal y define comportamientos y atributos específicos de los gatos
class Gato(nombre: String, edad: Int, raza: String) extends Animal(nombre, edad) {
    val razaGato: String = raza

    // Método sobreescrito para definir el ruido específico de un gato
    override def hacerRuido(): String = "Miau, Miau!"
}

// Clase Ave - Crea una clase de rasgos comunes de las aves
abstract class Ave(nombre: String, edad: Int) extends Animal(nombre, edad) {
    def volar(): String
}

// Clase Canario - Hereda de la clase Ave y define comportamientos y atributos específicos de los canarios
class Canario(nombre: String, edad: Int, raza: String) extends Ave(nombre, edad) {
    val razaCanario: String = raza

    // Método sobreescrito para definir la forma de volar específica de un canario
    override def volar(): String = "Pío, Pío! Estoy volando alto!"
}

// Clase Aguila - Hereda de la clase Ave y define comportamientos y atributos específicos de las águilas
class Aguila(nombre: String, edad: Int, raza: String) extends Ave(nombre, edad) {
    val razaAguila: String = raza

    // Método sobreescrito para definir la forma de volar específica de un águila
    override def volar(): String = "Graznido! Estoy volando majestuosamente!"
}

// Función principal - Crea objetos de tipo Perro, Gato, Canario y Águila y los invoca para mostrar sus comportamientos
object Main {
    def main(args: Array[String]): Unit = {
        // Crea un objeto de tipo Perro
        val perro = new Perro("Toby", 5, "Pastor Alemán")

        // Crea un objeto de tipo Gato
        val gato = new Gato("Michi", 3, "Persa")

        // Crea un objeto de tipo Canario
        val canario = new Canario("Piolín", 1, "Canario Timbrado")

        // Crea un objeto de tipo Águila
        val aguila = new Aguila("Aquila", 10, "Águila Calva")

        // Invoca el método hacerRuido() para cada objeto
        println(perro.hacerRuido())
        println(gato.hacerRuido())

        // Invoca el método volar() para cada objeto de tipo Ave
        println(canario.volar())
        println(aguila.volar())
    }
}
```

Explicación del código:

* **Clase Animal:** Define una clase base para todos los animales con atributos comunes como el nombre y la edad, y un método abstracto `hacerRuido()` para definir el ruido específico de cada animal.
* **Clases Perro y Gato:** Heredan de la clase Animal y definen comportamientos y atributos específicos de perros y gatos, incluyendo su raza y su ruido específico.
* **Clase Ave:** Define una clase base para todas las aves con atributos comunes como el nombre y la edad, y un método abstracto `volar()` para definir la forma de volar específica de cada ave.
* **Clases Canario y Águila:** Heredan de la clase Ave y definen comportamientos y atributos específicos de canarios y águilas, incluyendo su raza y su forma de volar específica.
* **Función principal:** Crea objetos de tipo Perro, Gato, Canario y Águila y los invoca para mostrar sus comportamientos, incluyendo el ruido que hacen y la forma en que vuelan (para los objetos de tipo Ave).