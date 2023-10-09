Claro! Aqui está um código complexo em Scala que envolve manipulação de listas, recursão e pattern matching:

```scala
object ComplexCode {
  def main(args: Array[String]): Unit = {
    val lista = List(1, 2, 3, 4, 5)
    val resultado = processarLista(lista)
    println(s"O resultado final é $resultado")
  }

  def processarLista(lista: List[Int]): Int = {
    lista match {
      case Nil => 0 // Caso base: lista vazia, retorna 0
      case head :: tail =>
        val resultadoParcial = operacaoComplexa(head)
        val resultadoRestante = processarLista(tail)
        resultadoParcial + resultadoRestante
    }
  }

  def operacaoComplexa(numero: Int): Int = {
    if (numero % 2 == 0) {
      val fatorial = calcularFatorial(numero)
      fatorial * 2
    } else {
      val fibonacci = calcularFibonacci(numero)
      fibonacci + 1
    }
  }

  def calcularFatorial(numero: Int): Int = {
    if (numero <= 1) 1
    else numero * calcularFatorial(numero - 1)
  }

  def calcularFibonacci(numero: Int): Int = {
    if (numero <= 1) numero
    else calcularFibonacci(numero - 1) + calcularFibonacci(numero - 2)
  }
}
```

Neste código, temos uma função `main` que cria uma lista de números inteiros e chama a função `processarLista` para processá-la. A função `processarLista` é recursiva e utiliza pattern matching para separar a lista em cabeça e cauda. 

Se a lista estiver vazia (caso base), a função retorna 0. Caso contrário, chama a função `operacaoComplexa` passando a cabeça da lista como argumento. A função `operacaoComplexa` verifica se o número é par ou ímpar e realiza diferentes operações com base nisso.

Se o número for par, a função calcula o fatorial do número e multiplica o resultado por 2. Se o número for ímpar, a função calcula o valor da sequência de Fibonacci para o número e soma 1 ao resultado.

No final, o código imprime o resultado final obtido após processar a lista.

Espero que esse código complexo em Scala atenda às suas expectativas!