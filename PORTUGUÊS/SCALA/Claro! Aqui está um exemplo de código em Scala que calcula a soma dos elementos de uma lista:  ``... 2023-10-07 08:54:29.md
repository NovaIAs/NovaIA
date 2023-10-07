Claro! Aqui está um exemplo de código em Scala que utiliza recursão para calcular o fatorial de um número inteiro:

```scala
object Fatorial {
  def main(args: Array[String]): Unit = {
    val numero = 5
    val resultado = calcularFatorial(numero)
    println(s"O fatorial de $numero é $resultado")
  }
  
  def calcularFatorial(n: Int): Int = {
    if (n == 0 || n == 1) {
      1
    } else {
      n * calcularFatorial(n - 1)
    }
  }
}
```

Neste exemplo, temos a definição de um objeto chamado "Fatorial" que contém uma função principal `main` e uma função auxiliar `calcularFatorial`. 

A função `main` é o ponto de entrada do programa. Neste caso, estamos definindo o número 5 como o valor para o qual queremos calcular o fatorial. Em seguida, chamamos a função `calcularFatorial` passando o número como argumento e armazenamos o resultado na variável `resultado`. Por fim, imprimimos o resultado na tela.

A função `calcularFatorial` é definida como recursiva. Ela recebe um parâmetro `n` do tipo inteiro e verifica se o valor de `n` é igual a 0 ou 1. Caso seja, retorna 1, pois o fatorial de 0 e 1 é sempre 1. Caso contrário, a função chama a si mesma passando `n - 1` como argumento e multiplica o resultado pelo valor de `n`. Isso é feito de forma recursiva até que `n` atinja 0 ou 1.

Espero que este exemplo atenda às suas expectativas! Se tiver mais alguma solicitação, por favor, me avise.