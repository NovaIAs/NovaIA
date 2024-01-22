**Problema:** Desarrollar un programa en Scala que genere una lista de números primos.

**Solución:**

```
object Primos {

  // Función auxiliar para comprobar si un número es primo
  private def esPrimo(n: Int): Boolean = {
    def esPrimoAux(d: Int): Boolean = {
      if (d > Math.sqrt(n)) true
      else if (n % d == 0) false
      else esPrimoAux(d + 1)
    }

    if (n == 1) false
    else esPrimoAux(2)
  }

  // Función principal que genera la lista de números primos
  def main(args: Array[String]): Unit = {
    val max = 1000 // Número máximo hasta el que generar primos

    // Crear la lista de números primos
    val primos = for (i <- 2 to max if esPrimo(i)) yield i

    // Imprimir la lista de números primos
    primos.foreach(println)
  }
}
```

**Explicación:**

* La clase `Primos` contiene la función principal `main` y la función auxiliar `esPrimo`.

* La función `esPrimo` comprueba si un número dado es primo. Utiliza una función auxiliar `esPrimoAux` que recorre los números desde 2 hasta la raíz cuadrada del número dado y comprueba si alguno de ellos divide al número. Si no encuentra ningún divisor, el número es primo.

* La función `main` genera la lista de números primos desde 2 hasta el número máximo especificado. Utiliza un bucle `for` con una cláusula `if` para filtrar los números que no son primos.

* La lista de números primos se imprime en la consola utilizando el método `foreach`.