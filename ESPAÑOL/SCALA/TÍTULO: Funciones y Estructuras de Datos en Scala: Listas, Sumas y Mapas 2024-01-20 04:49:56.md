```scala
// Crear una lista de números del 1 al 10.
val lista = (1 to 10).toList

// Imprimir la lista en consola.
println(lista)

// Crear una función que recibe una lista de números y retorna la suma de los números de la lista.
def sumarLista(lista: List[Int]): Int = {
  // Si la lista está vacía, retorna 0.
  if (lista.isEmpty) {
    0
  }
  // Si la lista no está vacía, retornar el primer elemento de la lista sumado a la suma del resto de la lista.
  else {
    lista.head + sumarLista(lista.tail)
  }
}

// Imprimir la suma de la lista en consola.
println(sumarLista(lista))

// Crear una función que recibe una lista de números y retorna un mapa con los números de la lista como claves y los cuadrados de los números de la lista como valores.
def cuadradosMapa(lista: List[Int]): Map[Int, Int] = {
  // Si la lista está vacía, retorna un mapa vacío.
  if (lista.isEmpty) {
    Map()
  }
  // Si la lista no está vacía, retornar un mapa con la clave como el primer elemento de la lista y el valor como el cuadrado del primer elemento de la lista, y el resto del mapa como el resultado de llamar a la función cuadradosMapa con el resto de la lista.
  else {
    Map(lista.head -> lista.head * lista.head) ++ cuadradosMapa(lista.tail)
  }
}

// Imprimir el mapa en consola.
println(cuadradosMapa(lista))
```

Explicación:

* La primera línea crea una lista de números del 1 al 10 usando el rango `1 to 10`.
* La segunda línea imprime la lista en consola usando `println`.
* La tercera línea crea una función llamada `sumarLista` que recibe una lista de números y retorna la suma de los números de la lista. La función usa recursión para sumar los números de la lista.
* La cuarta línea imprime la suma de la lista en consola.
* La quinta línea crea una función llamada `cuadradosMapa` que recibe una lista de números y retorna un mapa con los números de la lista como claves y los cuadrados de los números de la lista como valores. La función usa recursión para crear el mapa.
* La sexta línea imprime el mapa en consola.