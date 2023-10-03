Claro! Aqui está um exemplo de código em Scala que cria um programa para calcular o fatorial de um número:

```
object Fatorial {

  def calcularFatorial(n: Int): Int = {
    if (n == 0 || n == 1) {
      1
    } else {
      n * calcularFatorial(n - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val numero = 5
    val resultado = calcularFatorial(numero)
    println(s"O fatorial de $numero é $resultado")
  }
}
```

Neste código, definimos um objeto chamado "Fatorial". Dentro deste objeto, temos a função `calcularFatorial` que recebe um número inteiro `n` como parâmetro e retorna o fatorial desse número.

Primeiro, verificamos se o número é igual a 0 ou 1. Em ambos os casos, o fatorial é 1, então retornamos 1. Caso contrário, calculamos o fatorial de `n` multiplicando-o pelo fatorial de `n - 1`, ou seja, chamando recursivamente a função `calcularFatorial` com o valor `n - 1`.

Na função `main`, definimos uma variável chamada `numero` com o valor 5. Em seguida, chamamos a função `calcularFatorial` passando o valor de `numero` como argumento e armazenamos o resultado na variável `resultado`. Por fim, imprimimos uma mensagem com o valor do número e o resultado do fatorial utilizando a função `println`.

Este código é um exemplo simples e eficiente para calcular o fatorial de um número usando recursão em Scala.