```kotlin
// Envoltura de datos en clases

// Creamos una clase `Persona` con sus propiedades y métodos asociados:
class Persona(val nombre: String, val apellido: String, var edad: Int) {

    // Imprime los datos de la persona
    fun mostrarDatos() {
        println("Nombre: $nombre")
        println("Apellido: $apellido")
        println("Edad: $edad")
    }

    // Aumenta la edad de la persona en un año
    fun envejecer() {
        edad++
    }
}

// Creamos una instancia de la clase `Persona`
val persona1 = Persona("Juan", "Pérez", 25)

// Mostramos los datos de la persona antes de envejecer
persona1.mostrarDatos()

// Hacemos que la persona envejezca un año
persona1.envejecer()

// Mostramos los datos de la persona después de envejecer
persona1.mostrarDatos()

// Colecciones

// Creamos una lista de números enteros
val numeros = listOf(1, 2, 3, 4, 5)

// Iteramos sobre la lista y mostramos cada elemento
for (numero in numeros) {
    println(numero)
}

// Creamos un mapa de cadenas a enteros
val mapa = mapOf("uno" to 1, "dos" to 2, "tres" to 3)

// Iteramos sobre el mapa y mostramos cada clave y valor
for ((clave, valor) in mapa) {
    println("$clave: $valor")
}

// Funciones

// Creamos una función que calcula el factorial de un número entero
fun factorial(n: Int): Long {
    if (n == 0) {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// Llamamos a la función `factorial` para calcular el factorial de 5
val factorialDe5 = factorial(5)

// Mostramos el resultado
println("Factorial de 5: $factorialDe5")

// Clases abstractas e interfaces

// Creamos una interfaz `Forma` que define un método `calcularArea()`
interface Forma {
    fun calcularArea(): Double
}

// Creamos una clase abstracta `Figura` que implementa la interfaz `Forma`
abstract class Figura(val nombre: String): Forma {

    // Imprime el nombre de la figura
    fun mostrarNombre() {
        println("Nombre de la figura: $nombre")
    }

    // Método abstracto que debe ser implementado en las clases hijas
    abstract override fun calcularArea(): Double
}

// Creamos una clase hija `Rectángulo` que hereda de la clase `Figura`
class Rectángulo(nombre: String, val ancho: Double, val alto: Double): Figura(nombre) {

    // Implementamos el método `calcularArea()` para la clase `Rectángulo`
    override fun calcularArea(): Double {
        return ancho * alto
    }
}

// Creamos una instancia de la clase `Rectángulo`
val rectángulo1 = Rectángulo("Rectángulo 1", 5.0, 10.0)

// Mostramos el nombre del rectángulo
rectángulo1.mostrarNombre()

// Calculamos y mostramos el área del rectángulo
val areaRectángulo1 = rectángulo1.calcularArea()
println("Área del rectángulo: $areaRectángulo1")

// Manejo de excepciones

// Creamos una función que puede lanzar una excepción
fun dividir(numerador: Int, denominador: Int): Int {
    // Comprobamos si el denominador es igual a cero
    if (denominador == 0) {
        // Lanzamos una excepción de tipo `ArithmeticException`
        throw ArithmeticException("División por cero")
    } else {
        // Devolvemos el resultado de la división
        return numerador / denominador
    }
}

// Intentamos llamar a la función `dividir()` con un denominador igual a cero
try {
    val resultado = dividir(10, 0)
    println("Resultado de la división: $resultado")
} catch (e: ArithmeticException) {
    // Capturamos la excepción y mostramos un mensaje de error
    println("Error: ${e.message}")
}

// Subprocesos (Threads)

// Creamos una clase `Tarea` que implementa la interfaz `Runnable`
class Tarea: Runnable {

    // Sobrescribimos el método `run()` para definir lo que hará el subproceso
    override fun run() {
        // Realizamos alguna tarea
        for (i in 1..10) {
            println("Tarea $i")
            Thread.sleep(1000) // Esperamos un segundo entre cada tarea
        }
    }
}

// Creamos una instancia de la clase `Tarea`
val tarea1 = Tarea()

// Creamos un subproceso con la tarea
val subproceso1 = Thread(tarea1)

// Iniciamos el subproceso
subproceso1.start()

// Esperamos a que el subproceso termine
subproceso1.join()

// Mostramos un mensaje cuando el subproceso haya terminado
println("Todas las tareas completadas")

// Concurrencia (multihilo)

// Creamos una lista de números enteros
val listaNumeros = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// Creamos una función que calcula el cuadrado de un número
fun calcularCuadrado(numero: Int): Int {
    return numero * numero
}

// Creamos una lista de subprocesos
val subprocesos = mutableListOf<Thread>()

// Creamos un subproceso para cada número en la lista
for (numero in listaNumeros) {
    val tarea = object : Runnable {
        override fun run() {
            val cuadrado = calcularCuadrado(numero)
            println("El cuadrado de $numero es $cuadrado")
        }
    }
    val subproceso = Thread(tarea)
    subprocesos.add(subproceso)
}

// Iniciamos todos los subprocesos
for (subproceso in subprocesos) {
    subproceso.start()
}

// Esperamos a que todos los subprocesos terminen
for (subproceso in subprocesos) {
    subproceso.join()
}

// Mostramos un mensaje cuando todos los subprocesos hayan terminado
println("Todos los cuadrados calculados")

// Programación Funcional

// Creamos una lista de números enteros
val listaNumerosFuncionales = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// Calculamos la suma de los números en la lista usando funciones lambda
val suma = listaNumerosFuncionales.sumBy { it }

// Calculamos la lista de cuadrados de los números en la lista usando funciones lambda
val cuadrados = listaNumerosFuncionales.map { it * it }

// Filtramos la lista de números para obtener sólo los números pares usando funciones lambda
val pares = listaNumerosFuncionales.filter { it % 2 == 0 }

// Mostramos los resultados
println("Suma de los números: $suma")
println("Cuadrados de los números: $cuadrados")
println("Números pares: $pares")

// Programación Reactiva

// Creamos un flujo de datos observable
val observable = Observable.fromArray(1, 2, 3, 4, 5)

// Creamos un observador que recibirá los datos del flujo observable
val observador = object : Observer<Int> {
    override fun onNext(t: Int) {
        println("Recibido: $t")
    }

    override fun onError(e: Throwable) {
        println("Error: ${e.message}")
    }

    override fun onComplete() {
        println("Completado")
    }
}

// Suscribimos el observador al flujo observable
observable.subscribe(observador)

// Esperamos a que el flujo observable termine
observable.subscribeOn(Schedulers.io()).observeOn(AndroidSchedulers.mainThread()).subscribe()
```

Este código Kotlin es complejo y abarca una amplia variedad de conceptos y técnicas de programación, incluyendo envoltura de datos en clases, colecciones, funciones, clases abstractas e interfaces, manejo de excepciones, subprocesos, concurrencia, programación funcional y programación reactiva. El código incluye comentarios detallados en español que explican cada parte del código y su funcionamiento. Este código es un buen ejemplo de cómo combinar diferentes aspectos de Kotlin para crear aplicaciones complejas y eficientes.