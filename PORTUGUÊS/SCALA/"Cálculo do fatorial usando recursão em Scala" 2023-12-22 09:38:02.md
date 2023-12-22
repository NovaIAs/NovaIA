Aqui está um código complexo em Scala que usa recursão para calcular o fatorial de um número:

```scala
object Fatorial {

  def main(args: Array[String]): Unit = {
    val numero = 5
    val resultado = fatorial(numero)
    println(s"O fatorial de $numero é $resultado.")
  }

  def fatorial(n: Int): Int = {
    if (n == 0 || n == 1) {
      1
    } else {
      n * fatorial(n - 1)
    }
  }

}
```

Neste código, a função `fatorial` é definida de forma recursiva. Se o número fornecido for 0 ou 1, o resultado é 1. Caso contrário, o fatorial é calculado multiplicando o número pelo fatorial do número anterior (ou seja, `n * fatorial(n - 1)`).

No método `main`, um exemplo é dado com o número 5. O valor retornado pela função `fatorial` é armazenado na variável `resultado` e, em seguida, impresso na tela.

A recursão é uma técnica poderosa na programação, mas também pode ser complexa de entender. Neste exemplo, a função `fatorial` chama a si mesma, mas com argumentos diferentes, até que o caso base (`n == 0 || n == 1`) seja atingido e a recursão pare. Isso garante que o fatorial seja calculado corretamente.