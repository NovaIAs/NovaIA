```scala
// Importamos las librerías necesarias para nuestro código
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// Definimos una función para generar números aleatorios
def generarNumeroAleatorio(): Int = {
  // Usamos la clase 'Random' para generar un número aleatorio
  val random = new Random()

  // Generamos un número aleatorio entre 1 y 100
  random.nextInt(100) + 1
}

// Definimos una función para calcular la media de un array de números
def calcularMedia(numeros: Array[Int]): Double = {
  // Sumamos todos los números del array
  val sumaNumeros = numeros.sum

  // Dividimos la suma de los números por el número de elementos del array
  sumaNumeros / numeros.length
}

// Definimos una función para encontrar el máximo de un array de números
def encontrarMaximo(numeros: Array[Int]): Int = {
  // Inicializamos la variable 'maximo' con el primer elemento del array
  var maximo = numeros(0)

  // Recorremos el array y actualizamos la variable 'maximo' si encontramos un número mayor
  for (numero <- numeros) {
    if (numero > maximo) {
      maximo = numero
    }
  }

  // Devolvemos la variable 'maximo'
  maximo
}

// Definimos una función para encontrar el mínimo de un array de números
def encontrarMinimo(numeros: Array[Int]): Int = {
  // Inicializamos la variable 'minimo' con el primer elemento del array
  var minimo = numeros(0)

  // Recorremos el array y actualizamos la variable 'minimo' si encontramos un número menor
  for (numero <- numeros) {
    if (numero < minimo) {
      minimo = numero
    }
  }

  // Devolvemos la variable 'minimo'
  minimo
}

// Creamos un array de 10 números aleatorios
val numeros = new ArrayBuffer[Int]()

for (i <- 1 to 10) {
  numeros += generarNumeroAleatorio()
}

// Calculamos la media, el máximo y el mínimo del array de números
val media = calcularMedia(numeros.toArray)
val maximo = encontrarMaximo(numeros.toArray)
val minimo = encontrarMinimo(numeros.toArray)

// Imprimimos los resultados
println("Los números generados son: " + numeros.mkString(", "))
println("La media de los números es: " + media)
println("El máximo de los números es: " + maximo)
println("El mínimo de los números es: " + minimo)