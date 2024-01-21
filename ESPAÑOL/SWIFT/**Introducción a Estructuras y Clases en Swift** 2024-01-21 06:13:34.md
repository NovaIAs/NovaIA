```swift
// Creación de una estructura personalizada para representar una persona.
struct Persona {
    var nombre: String
    var edad: Int
    var ciudad: String

    // Función para saludar.
    func saludar() {
        print("Hola, mi nombre es \(nombre) y tengo \(edad) años.")
    }

    // Función para indicar la ciudad donde vive esta persona.
    func ciudadDeResidencia() {
        print("Vivo en \(ciudad).")
    }
}

// Creando algunos objetos de la estructura Persona.
let juan = Persona(nombre: "Juan", edad: 25, ciudad: "Madrid")
let maria = Persona(nombre: "María", edad: 30, ciudad: "Barcelona")
let pedro = Persona(nombre: "Pedro", edad: 35, ciudad: "Sevilla")

// Imprimiendo el saludo de cada persona.
juan.saludar()
maria.saludar()
pedro.saludar()

// Imprimiendo la ciudad de residencia de cada persona.
juan.ciudadDeResidencia()
maria.ciudadDeResidencia()
pedro.ciudadDeResidencia()

// Creando una matriz con las personas.
let personas = [juan, maria, pedro]

// Recorriendo la matriz de personas e imprimiendo sus nombres.
for persona in personas {
    print(persona.nombre)
}

// Creando una clase personalizada para representar una mascota.
class Mascota {
    var nombre: String
    var tipo: String

    // Función para pedir comida.
    func pedirComida() {
        print("Tengo hambre, ¿me das de comer?")
    }

    // Función para jugar.
    func jugar() {
        print("Vamos a jugar!")
    }

    // Función para imprimir una descripción de la mascota.
    func descripcion() {
        print("Soy un(a) \(tipo) llamado(a) \(nombre).")
    }
}

// Creando algunos objetos de la clase Mascota.
let perro = Mascota(nombre: "Firulais", tipo: "perro")
let gato = Mascota(nombre: "Michi", tipo: "gato")
let pez = Mascota(nombre: "Nemo", tipo: "pez")

// Imprimiendo una descripción de cada mascota.
perro.descripcion()
gato.descripcion()
pez.descripcion()

// Imprimiendo si la mascota pide comida.
perro.pedirComida()
gato.pedirComida()
pez.pedirComida()

// Imprimiendo si la mascota juega.
perro.jugar()
gato.jugar()
pez.jugar()

// Creando un conjunto con las mascotas.
let mascotas = Set([perro, gato, pez])

// Recorriendo el conjunto de mascotas e imprimiendo sus nombres.
for mascota in mascotas {
    print(mascota.nombre)
}
```

Explicación del código:

* Se define una estructura `Persona` con los atributos `nombre`, `edad` y `ciudad`, y dos métodos, `saludar()` y `ciudadDeResidencia()`.
* Se crean tres objetos de la estructura `Persona` y se imprime el saludo y la ciudad de cada uno de ellos.
* Se define una clase `Mascota` con los atributos `nombre` y `tipo`, y tres métodos, `pedirComida()`, `jugar()` y `descripcion()`.
* Se crean tres objetos de la clase `Mascota` y se imprime una descripción de cada uno de ellos.
* Se imprimen los nombres de las mascotas si piden comida o si juegan.
* Se crea un conjunto con las mascotas y se imprime el nombre de cada mascota.