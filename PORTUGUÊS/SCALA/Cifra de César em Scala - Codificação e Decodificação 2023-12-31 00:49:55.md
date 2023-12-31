Claro! Vou criar um código em Scala que realiza a codificação de uma cifra de César. A cifra de César é um tipo de cifra de substituição onde cada letra do texto original é substituída por outra letra que está um número fixo de posições à frente no alfabeto.

```scala
object CifraDeCesar {

  def cifrar(texto: String, deslocamento: Int): String = {
    val letras = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val tamanhoAlfabeto = letras.size
    val deslocamentoNormalizado = deslocamento % tamanhoAlfabeto

    def cifrarCaracter(caracter: Char): Char = {
      val indice = letras.indexOf(caracter)
      if (indice == -1) {
        caracter
      } else {
        val novoIndice = (indice + deslocamentoNormalizado) % tamanhoAlfabeto
        letras(novoIndice)
      }
    }

    texto.map(cifrarCaracter).mkString
  }

  def decifrar(texto: String, deslocamento: Int): String = {
    cifrar(texto, -deslocamento)
  }

  def main(args: Array[String]): Unit = {
    val textoOriginal = "Scala é uma linguagem de programação moderna e poderosa."
    val deslocamento = 5

    val textoCifrado = cifrar(textoOriginal, deslocamento)
    val textoDecifrado = decifrar(textoCifrado, deslocamento)

    println("Texto original: " + textoOriginal)
    println("Texto cifrado: " + textoCifrado)
    println("Texto decifrado: " + textoDecifrado)
  }
}
```

Este código em Scala implementa a cifra de César de forma simples e eficiente. Ele possui três métodos principais:

1. O método `cifrar` recebe uma string de texto e um valor de deslocamento como parâmetros. Ele percorre cada caractere do texto, verificando se é uma letra válida. Se for, calcula o novo índice da letra cifrada com base no deslocamento e o tamanho do alfabeto. Em seguida, retorna o caractere cifrado correspondente. Caso contrário, mantém o caractere original. Ao final, retorna a string cifrada.

2. O método `decifrar` utiliza o método `cifrar` com um deslocamento negativo para decifrar uma string cifrada.

3. O método `main` é onde a execução do código começa. Ele define um texto original e um valor de deslocamento. Em seguida, chama o método `cifrar` para cifrar o texto original com o deslocamento especificado. Depois, chama o método `decifrar` para decifrar o texto cifrado. Por fim, imprime na tela o texto original, o texto cifrado e o texto decifrado.

Você pode executar este código em um ambiente Scala para ver a cifra de César em ação.