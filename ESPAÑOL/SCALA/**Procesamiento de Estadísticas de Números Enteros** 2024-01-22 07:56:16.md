```scala
// Crear una clase con un constructor que reciba una lista de números enteros
class SumaDeNúmeros(números: List[Int]) {

  // Definir una función que calcule la suma de los números en la lista
  def sumar(): Int = {
    // Utilizar la función `foldLeft` para sumar todos los números en la lista
    números.foldLeft(0)((suma, número) => suma + número)
  }

  // Definir una función que calcule el promedio de los números en la lista
  def promediar(): Double = {
    // Utilizar la función `sumar` para obtener la suma de los números
    val suma = sumar()

    // Dividir la suma por el número de elementos en la lista para obtener el promedio
    suma.toDouble / números.size
  }

  // Definir una función que calcule la mediana de los números en la lista
  def medianizar(): Double = {
    // Ordenar la lista en orden ascendente
    val númerosOrdenados = números.sorted

    // Si la lista tiene un número impar de elementos, la mediana es el elemento central
    if (númerosOrdenados.size % 2 == 1) {
      númerosOrdenados(númerosOrdenados.size / 2)
    }
    // Si la lista tiene un número par de elementos, la mediana es el promedio de los dos elementos centrales
    else {
      val índiceMedio1 = númerosOrdenados.size / 2 - 1
      val índiceMedio2 = númerosOrdenados.size / 2
      (númerosOrdenados(índiceMedio1) + númerosOrdenados(índiceMedio2)) / 2.0
    }
  }

  // Definir una función que imprima por pantalla la suma, el promedio y la mediana de los números en la lista
  def imprimirResultados(): Unit = {
    println("Suma: " + sumar())
    println("Promedio: " + promediar())
    println("Mediana: " + medianizar())
  }
}

// Crear una lista de números enteros
val números = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// Crear un objeto de la clase SumaDeNúmeros
val sumaDeNúmeros = new SumaDeNúmeros(números)

// Llamar a la función `imprimirResultados` para imprimir los resultados por pantalla
sumaDeNúmeros.imprimirResultados()
```

Explicación:

* La clase `SumaDeNúmeros` tiene un constructor que recibe una lista de números enteros.
* La clase define tres funciones:
    * `sumar()`: calcula la suma de los números en la lista.
    * `promediar()`: calcula el promedio de los números en la lista.
    * `medianizar()`: calcula la mediana de los números en la lista.
* La clase también define una función `imprimirResultados()` que imprime los resultados por pantalla.
* Se crea una lista de números enteros y se crea un objeto de la clase `SumaDeNúmeros` con la lista.
* Se llama a la función `imprimirResultados()` para imprimir los resultados por pantalla.