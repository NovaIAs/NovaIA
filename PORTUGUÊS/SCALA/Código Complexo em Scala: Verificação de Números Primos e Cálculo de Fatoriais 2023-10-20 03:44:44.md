Claro! Aqui está um código complexo em Scala:

```scala
object ProgramaComplexo {

  def main(args: Array[String]): Unit = {
    // Definindo uma lista de números primos
    val numerosPrimos = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)

    // Função para verificar se um número é primo
    def isPrimo(numero: Int): Boolean = {
      if (numero < 2) false
      else {
        val raizQuadrada = Math.sqrt(numero).toInt
        def isDivisivel(divisor: Int): Boolean = {
          if (divisor > raizQuadrada) true
          else if (numero % divisor == 0) false
          else isDivisivel(divisor + 1)
        }
        isDivisivel(2)
      }
    }

    // Filtrando os números primos da lista
    val numerosFiltrados = numerosPrimos.filter(isPrimo)

    // Função para calcular o fatorial de um número
    def calcularFatorial(numero: Int): Int = {
      if (numero == 0) 1
      else numero * calcularFatorial(numero - 1)
    }

    // Calculando o fatorial de cada número primo filtrado
    val fatoriais = numerosFiltrados.map(calcularFatorial)

    // Imprimindo os resultados
    println("Números primos filtrados:")
    numerosFiltrados.foreach(println)

    println("Fatoriais dos números primos filtrados:")
    fatoriais.foreach(println)
  }
}
```

Este código em Scala demonstra algumas funcionalidades interessantes da linguagem. Primeiro, definimos uma lista de números primos. Em seguida, criamos uma função `isPrimo` para verificar se um número é primo, utilizando recursão para verificar se ele é divisível por algum número entre 2 e a raiz quadrada do número em questão.

A seguir, filtramos os números primos da lista original utilizando a função `filter`, passando a função `isPrimo` como argumento. Assim, obtemos uma nova lista apenas com os números primos.

Em seguida, definimos uma função `calcularFatorial` para calcular o fatorial de um número utilizando recursão. Utilizamos a função `map` para aplicar essa função a cada elemento da lista de números primos filtrados, obtendo uma lista com os fatoriais correspondentes.

Por fim, imprimimos os números primos filtrados e seus respectivos fatoriais utilizando a função `foreach`.

Espero que este código complexo em Scala tenha sido útil para você!