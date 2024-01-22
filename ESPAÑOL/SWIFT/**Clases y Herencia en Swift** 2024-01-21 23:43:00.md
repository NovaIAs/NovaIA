```swift
// Definición de una clase Persona con propiedades y métodos
class Persona {
    var nombre: String
    var apellido: String
    var edad: Int
    
    init(nombre: String, apellido: String, edad: Int) {
        self.nombre = nombre
        self.apellido = apellido
        self.edad = edad
    }
    
    func saludar() {
        print("Hola, mi nombre es \(nombre) \(apellido) y tengo \(edad) años.")
    }
}

// Definición de una clase Estudiante que hereda de Persona y añade propiedades y métodos específicos
class Estudiante: Persona {
    var matricula: String
    var carrera: String
    
    init(nombre: String, apellido: String, edad: Int, matricula: String, carrera: String) {
        self.matricula = matricula
        self.carrera = carrera
        
        // Llamada al inicializador de la clase padre
        super.init(nombre: nombre, apellido: apellido, edad: edad)
    }
    
    func estudiar() {
        print("Estoy estudiando \(carrera).")
    }
}

// Definición de una clase Profesor que hereda de Persona y añade propiedades y métodos específicos
class Profesor: Persona {
    var materia: String
    var departamento: String
    
    init(nombre: String, apellido: String, edad: Int, materia: String, departamento: String) {
        self.materia = materia
        self.departamento = departamento
        
        // Llamada al inicializador de la clase padre
        super.init(nombre: nombre, apellido: apellido, edad: edad)
    }
    
    func darClase() {
        print("Estoy dando clase de \(materia).")
    }
}

// Creación de objetos Persona, Estudiante y Profesor
let persona1 = Persona(nombre: "Juan", apellido: "García", edad: 30)
let estudiante1 = Estudiante(nombre: "María", apellido: "López", edad: 20, matricula: "123456", carrera: "Ingeniería Informática")
let profesor1 = Profesor(nombre: "Pedro", apellido: "Sánchez", edad: 40, materia: "Matemáticas", departamento: "Ciencias")

// Llamada a los métodos de cada objeto
persona1.saludar()
estudiante1.saludar()
estudiante1.estudiar()
profesor1.saludar()
profesor1.darClase()
```

Explicación del código:

* Se definen tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` es la clase base, mientras que las clases `Estudiante` y `Profesor` heredan de la clase `Persona`.
* Cada clase tiene sus propias propiedades y métodos específicos. Por ejemplo, la clase `Estudiante` tiene propiedades como `matricula` y `carrera`, y métodos como `estudiar()`. La clase `Profesor` tiene propiedades como `materia` y `departamento`, y métodos como `darClase()`.
* Se crean objetos de cada clase y se llaman a los métodos de cada objeto.