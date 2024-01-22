```swift
// Definición de una clase llamada "Persona" con propiedades "nombre" y "edad".
class Persona {
    var nombre: String
    var edad: Int
    
    init(nombre: String, edad: Int) {
        self.nombre = nombre
        self.edad = edad
    }
    
    // Definición de un método llamado "hablar" que imprime el nombre y la edad de la persona.
    func hablar() {
        print("Mi nombre es \(nombre) y mi edad es \(edad).")
    }
}

// Definición de una clase llamada "Estudiante" que hereda de la clase "Persona" y añade la propiedad "calificaciones".
class Estudiante: Persona {
    var calificaciones: [Int]
    
    init(nombre: String, edad: Int, calificaciones: [Int]) {
        self.calificaciones = calificaciones
        super.init(nombre: nombre, edad: edad)
    }
    
    // Definición de un método llamado "estudiar" que imprime las calificaciones del estudiante.
    func estudiar() {
        print("Mis calificaciones son \(calificaciones).")
    }
}

// Definición de una clase llamada "Profesor" que hereda de la clase "Persona" y añade la propiedad "materias".
class Profesor: Persona {
    var materias: [String]
    
    init(nombre: String, edad: Int, materias: [String]) {
        self.materias = materias
        super.init(nombre: nombre, edad: edad)
    }
    
    // Definición de un método llamado "enseñar" que imprime las materias que imparte el profesor.
    func enseñar() {
        print("Imparto las siguientes materias: \(materias).")
    }
}

// Creación de una instancia de la clase "Estudiante" llamada "Juan".
let juan = Estudiante(nombre: "Juan", edad: 18, calificaciones: [9, 8, 7])

// Creación de una instancia de la clase "Profesor" llamada "María".
let maria = Profesor(nombre: "María", edad: 40, materias: ["Matemáticas", "Física", "Química"])

// Invocación del método "hablar" para el objeto "juan".
juan.hablar()

// Invocación del método "estudiar" para el objeto "juan".
juan.estudiar()

// Invocación del método "enseñar" para el objeto "maria".
maria.enseñar()
```

Explicación del código:

* Se definen tres clases: "Persona", "Estudiante" y "Profesor".
* La clase "Persona" tiene las propiedades "nombre" y "edad", y un método llamado "hablar" que imprime el nombre y la edad de la persona.
* La clase "Estudiante" hereda de la clase "Persona" y añade la propiedad "calificaciones". Además, tiene un método llamado "estudiar" que imprime las calificaciones del estudiante.
* La clase "Profesor" hereda de la clase "Persona" y añade la propiedad "materias". Además, tiene un método llamado "enseñar" que imprime las materias que imparte el profesor.
* Se crean dos instancias de la clase "Estudiante" y "Profesor", llamadas "Juan" y "María", respectivamente.
* Se invocan los métodos "hablar", "estudiar" y "enseñar" para los objetos "Juan" y "María", respectivamente.