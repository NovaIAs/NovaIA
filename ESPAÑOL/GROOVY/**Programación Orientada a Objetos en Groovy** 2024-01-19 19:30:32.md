```groovy
// Definición de clases y métodos

class Persona {
    String nombre
    String apellido
    int edad

    String hablar() {
        "Hola, mi nombre es $nombre $apellido."
    }
}

class Estudiante extends Persona {
    String carrera
    double promedio

    String estudiar() {
        "Estoy estudiando $carrera."
    }
}

class Profesor extends Persona {
    String materia
    int añosDeExperiencia

    String enseñar() {
        "Estoy enseñando $materia."
    }
}

// Creación de objetos

Persona persona1 = new Persona(nombre: "Juan", apellido: "García", edad: 25)
Estudiante estudiante1 = new Estudiante(nombre: "María", apellido: "Pérez", edad: 20, carrera: "Ingeniería en Computación", promedio: 4.0)
Profesor profesor1 = new Profesor(nombre: "Carlos", apellido: "Rodríguez", edad: 40, materia: "Matemáticas", añosDeExperiencia: 15)

// Utilización de métodos

println persona1.hablar()
println estudiante1.hablar()
println estudiante1.estudiar()
println profesor1.hablar()
println profesor1.enseñar()

// Listas y bucles

List<Persona> personas = [persona1, estudiante1, profesor1]

for (Persona persona in personas) {
    println persona.hablar()
}

// Expresiones lambda y funciones anónimas

personas.each { persona ->
    println persona.hablar()
}

double promedioEdades = personas.sum { persona -> persona.edad } / personas.size()

println "Promedio de edades: $promedioEdades"

// Manejo de excepciones

try {
    int resultado = 10 / 0
} catch (ArithmeticException e) {
    println "Error: no se puede dividir por cero."
}

// Entrada y salida de datos

print "Ingrese su nombre: "
String nombreIngresado = System.in.newReader().readLine()

println "Hola, $nombreIngresado."
```

Explicación del código:

* Definición de clases y métodos: se definen las clases `Persona`, `Estudiante` y `Profesor` con propiedades y métodos.
* Creación de objetos: se crean los objetos `persona1`, `estudiante1` y `profesor1` de las clases correspondientes.
* Utilización de métodos: se utilizan los métodos de los objetos creados para imprimir información.
* Listas y bucles: se crea una lista de personas y se utiliza un bucle para iterar sobre la lista e imprimir la información de cada persona.
* Expresiones lambda y funciones anónimas: se utilizan expresiones lambda y funciones anónimas para procesar la lista de personas y calcular el promedio de las edades.
* Manejo de excepciones: se utiliza el manejo de excepciones para capturar una excepción de división por cero.
* Entrada y salida de datos: se utiliza la entrada estándar para leer el nombre del usuario y la salida estándar para imprimir un mensaje de bienvenida.