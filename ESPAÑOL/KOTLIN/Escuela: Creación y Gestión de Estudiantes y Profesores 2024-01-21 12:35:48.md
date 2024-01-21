// Clase para representar a un estudiante
class Estudiante {
    var nombre: String
    var apellido: String
    var edad: Int

    constructor(nombre: String, apellido: String, edad: Int) {
        this.nombre = nombre
        this.apellido = apellido
        this.edad = edad
    }

    // Función para obtener el nombre completo del estudiante
    fun getNombreCompleto(): String {
        return "$nombre $apellido"
    }
}

// Clase para representar a un profesor
class Profesor {
    var nombre: String
    var apellido: String
    var materia: String

    constructor(nombre: String, apellido: String, materia: String) {
        this.nombre = nombre
        this.apellido = apellido
        this.materia = materia
    }

    // Función para obtener el nombre completo del profesor
    fun getNombreCompleto(): String {
        return "$nombre $apellido"
    }
}

// Clase para representar a una escuela
class Escuela {
    var nombre: String
    var direccion: String
    var estudiantes: MutableList<Estudiante> = mutableListOf()
    var profesores: MutableList<Profesor> = mutableListOf()

    constructor(nombre: String, direccion: String) {
        this.nombre = nombre
        this.direccion = direccion
    }

    // Función para agregar un estudiante a la escuela
    fun agregarEstudiante(estudiante: Estudiante) {
        estudiantes.add(estudiante)
    }

    // Función para agregar un profesor a la escuela
    fun agregarProfesor(profesor: Profesor) {
        profesores.add(profesor)
    }

    // Función para obtener el número total de estudiantes en la escuela
    fun obtenerNumeroEstudiantes(): Int {
        return estudiantes.size
    }

    // Función para obtener el número total de profesores en la escuela
    fun obtenerNumeroProfesores(): Int {
        return profesores.size
    }

    // Función para obtener el nombre de la escuela
    fun obtenerNombre(): String {
        return nombre
    }

    // Función para obtener la dirección de la escuela
    fun obtenerDireccion(): String {
        return direccion
    }
}

// Función principal del programa
fun main(args: Array<String>) {
    // Crear una nueva escuela
    val escuela = Escuela("Escuela Secundaria", "Calle Principal 123")

    // Agregar algunos estudiantes a la escuela
    escuela.agregarEstudiante(Estudiante("Juan", "Pérez", 16))
    escuela.agregarEstudiante(Estudiante("María", "García", 17))
    escuela.agregarEstudiante(Estudiante("Pedro", "Rodríguez", 18))

    // Agregar algunos profesores a la escuela
    escuela.agregarProfesor(Profesor("Ana", "López", "Matemáticas"))
    escuela.agregarProfesor(Profesor("José", "Martínez", "Ciencias"))
    escuela.agregarProfesor(Profesor("Luis", "Hernández", "Historia"))

    // Mostrar el número de estudiantes y profesores en la escuela
    println("Número de estudiantes: ${escuela.obtenerNumeroEstudiantes()}")
    println("Número de profesores: ${escuela.obtenerNumeroProfesores()}")

    // Mostrar el nombre y la dirección de la escuela
    println("Nombre de la escuela: ${escuela.obtenerNombre()}")
    println("Dirección de la escuela: ${escuela.obtenerDireccion()}")

    // Mostrar los nombres de los estudiantes y profesores de la escuela
    println("Estudiantes:")
    for (estudiante in escuela.estudiantes) {
        println(estudiante.getNombreCompleto())
    }

    println("Profesores:")
    for (profesor in escuela.profesores) {
        println(profesor.getNombreCompleto())
    }
}