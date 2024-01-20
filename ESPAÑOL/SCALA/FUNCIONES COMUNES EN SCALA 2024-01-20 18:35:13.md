```scala
// Definimos una función que toma una lista de enteros y devuelve la suma de todos ellos
def sumarLista(lista: List[Int]): Int = {
  // Si la lista está vacía, devolvemos 0
  if (lista.isEmpty) {
    0
  }
  // En caso contrario, devolvemos la suma del primer elemento de la lista y el resto de la lista
  else {
    lista.head + sumarLista(lista.tail)
  }
}

// Definimos una función que toma una lista de cadenas y devuelve una cadena con todas las cadenas concatenadas
def concatenarLista(lista: List[String]): String = {
  // Si la lista está vacía, devolvemos una cadena vacía
  if (lista.isEmpty) {
    ""
  }
  // En caso contrario, devolvemos la primera cadena de la lista concatenada con el resto de la lista
  else {
    lista.head + concatenarLista(lista.tail)
  }
}

// Definimos una función que toma un número entero y devuelve su factorial
def factorial(n: Int): Int = {
  // Si n es 0, devolvemos 1
  if (n == 0) {
    1
  }
  // En caso contrario, devolvemos n multiplicado por el factorial de n-1
  else {
    n * factorial(n-1)
  }
}

// Definimos una función que toma una lista de números enteros y devuelve una lista con los números ordenados de menor a mayor
def ordenarLista(lista: List[Int]): List[Int] = {
  // Si la lista está vacía, devolvemos una lista vacía
  if (lista.isEmpty) {
    List()
  }
  // En caso contrario, devolvemos una lista con el primer elemento de la lista ordenado, seguido del resto de la lista ordenada
  else {
    val min = lista.min
    min :: ordenarLista(lista.filterNot(_ == min))
  }
}

// Definimos una función que toma una lista de números enteros y devuelve una lista con los números pares
def filtrarPares(lista: List[Int]): List[Int] = {
  // Si la lista está vacía, devolvemos una lista vacía
  if (lista.isEmpty) {
    List()
  }
  // En caso contrario, devolvemos una lista con el primer elemento de la lista si es par, seguido del resto de la lista filtrada
  else {
    val head = lista.head
    if (head % 2 == 0) {
      head :: filtrarPares(lista.tail)
    } else {
      filtrarPares(lista.tail)
    }
  }
}

// Definimos una función que toma una lista de números enteros y devuelve una lista con los números impares
def filtrarImpares(lista: List[Int]): List[Int] = {
  // Si la lista está vacía, devolvemos una lista vacía
  if (lista.isEmpty) {
    List()
  }
  // En caso contrario, devolvemos una lista con el primer elemento de la lista si es impar, seguido del resto de la lista filtrada
  else {
    val head = lista.head
    if (head % 2 != 0) {
      head :: filtrarImpares(lista.tail)
    } else {
      filtrarImpares(lista.tail)
    }
  }
}

// Definimos una función que toma una lista de números enteros y devuelve una lista con los números primos
def filtrarPrimos(lista: List[Int]): List[Int] = {
  // Si la lista está vacía, devolvemos una lista vacía
  if (lista.isEmpty) {
    List()
  }
  // En caso contrario, devolvemos una lista con el primer elemento de la lista si es primo, seguido del resto de la lista filtrada
  else {
    val head = lista.head
    if (esPrimo(head)) {
      head :: filtrarPrimos(lista.tail)
    } else {
      filtrarPrimos(lista.tail)
    }
  }
}

// Definimos una función que toma un número entero y devuelve si es primo
def esPrimo(n: Int): Boolean