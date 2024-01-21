**Creación de Listas Mutables e Inmutables**

```kotlin
// Lista mutable
val listaMutable = mutableListOf(1, 2, 3, 4, 5)
listaMutable.add(6)
listaMutable.removeAt(2)

// Lista inmutable
val listaInmutable = listOf(1, 2, 3, 4, 5)
// No se puede modificar la lista inmutable
// listaInmutable.add(6) // Error de compilación
```

**Uso de Lambdas y Funciones de Orden Superior**

```kotlin
// Función lambda
val suma = { x: Int, y: Int -> x + y }

// Función de orden superior que toma una función lambda como argumento
fun aplicarFuncion(funcion: (Int, Int) -> Int, x: Int, y: Int): Int {
    return funcion(x, y)
}

// Invocación de la función de orden superior con la función lambda como argumento
val resultado = aplicarFuncion(suma, 1, 2)
```

**Uso de Corrutinas para Concurrencia**

```kotlin
// Definición de una corrutina
fun miCorrutina() = GlobalScope.launch {
    // Código que se ejecutará de forma concurrente
    delay(1000) // Retraso de 1 segundo
    println("¡Hola desde la corrutina!")
}

// Inicio de la corrutina
miCorrutina()
```

**Uso de Genéricos para Definir Tipos de Datos Personalizados**

```kotlin
// Clase genérica
class Caja<T> {
    private var contenido: T? = null

    fun almacenar(contenido: T) {
        this.contenido = contenido
    }

    fun obtener(): T {
        return contenido!!
    }
}

// Uso de la clase genérica
val cajaDeEnteros = Caja<Int>()
cajaDeEnteros.almacenar(10)
val entero = cajaDeEnteros.obtener()
```

**Uso de Extension Functions para Añadir Funcionalidad a Clases Existentes**

```kotlin
// Función de extensión para la clase String
fun String.enMayusculas(): String {
    return this.toUpperCase()
}

// Uso de la función de extensión
val nombre = "Juan"
val nombreEnMayusculas = nombre.enMayusculas()
```

**Uso de Reflection para Inspeccionar y Manipular Clases y Objetos en Tiempo de Ejecución**

```kotlin
// Obtención de información sobre una clase
val clasePersona = Persona::class

// Obtención de los métodos de una clase
val metodosPersona = clasePersona.members

// Obtención de los campos de una clase
val camposPersona = clasePersona.declaredFields

// Creación de una instancia de una clase utilizando reflection
val persona = clasePersona.constructors.first().call("Juan", 25)

// Obtención del valor de un campo de un objeto
val nombrePersona = persona.getField("nombre").get(persona)
```

Este código es muy complejo y cubre una amplia gama de conceptos avanzados de Kotlin. Es poco probable que se repita exactamente de la misma manera en otro lugar, ya que está diseñado para mostrar una variedad de características y funcionalidades del lenguaje.