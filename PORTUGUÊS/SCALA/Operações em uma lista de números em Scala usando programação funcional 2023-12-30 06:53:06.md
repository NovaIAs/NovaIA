Claro! Aqui está um código complexo em Scala que utiliza programação funcional para realizar uma série de operações em uma lista de números:

```scala
object OperacoesLista {
  def main(args: Array[String]): Unit = {
    val numeros = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    
    val soma = numeros.reduce(_ + _)
    println("A soma dos números é: " + soma)
    
    val media = soma.toDouble / numeros.length
    println("A média dos números é: " + media)
    
    val numerosPares = numeros.filter(_ % 2 == 0)
    println("Os números pares são: " + numerosPares.mkString(", "))
    
    val numerosImpares = numeros.filter(_ % 2 != 0)
    println("Os números ímpares são: " + numerosImpares.mkString(", "))
    
    val numerosDobrados = numeros.map(_ * 2)
    println("Os números dobrados são: " + numerosDobrados.mkString(", "))
    
    val numerosOrdenados = numeros.sortWith(_ > _)
    println("Os números em ordem decrescente são: " + numerosOrdenados.mkString(", "))
    
    val primeiroNumero = numeros.head
    println("O primeiro número da lista é: " + primeiroNumero)
    
    val ultimoNumero = numeros.last
    println("O último número da lista é: " + ultimoNumero)
  }
}
```

Neste código, a função `main` é o ponto de entrada do programa. Começamos definindo uma lista de números de 1 a 10. Em seguida, utilizamos as seguintes operações:

- `reduce` para calcular a soma de todos os números da lista.
- `toDouble` para converter a soma em um número de ponto flutuante e calcular a média, dividindo-a pelo tamanho da lista.
- `filter` para filtrar os números pares e os números ímpares em listas separadas.
- `map` para dobrar cada número da lista.
- `sortWith` para ordenar os números em ordem decrescente.
- `head` e `last` para obter o primeiro e o último número da lista, respectivamente.

Por fim, imprimimos os resultados na tela.