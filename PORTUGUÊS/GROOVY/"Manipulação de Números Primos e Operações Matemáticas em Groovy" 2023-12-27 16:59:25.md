Certamente! Aqui está um código Groovy complexo que envolve uma série de operações matemáticas e manipulação de listas:

```groovy
// Definindo uma função personalizada que retorna o fatorial de um número
def fatorial(n) {
  if (n == 0 || n == 1) {
    return 1
  } else {
    return n * fatorial(n - 1)
  }
}

// Definindo uma classe customizada para manipular listas de números primos
class ListaPrimos {
  List<Integer> primos = []

  // Método para adicionar um número primo à lista
  def adicionarPrimo(n) {
    if (isPrimo(n)) {
      primos.add(n)
    } else {
      println("O número informado não é primo.")
    }
  }

  // Método para verificar se um número é primo
  def isPrimo(n) {
    if (n <= 1) {
      return false
    }

    for (int i = 2; i < n; i++) {
      if (n % i == 0) {
        return false
      }
    }

    return true
  }

  // Método para calcular a soma de todos os números primos na lista
  def somaPrimos() {
    def soma = primos.sum()
    println("A soma dos números primos na lista é: " + soma)
  }

  // Método para calcular a média dos fatoriais de todos os números primos na lista
  def mediaFatoriais() {
    def somaFatoriais = 0
    for (int i = 0; i < primos.size(); i++) {
      somaFatoriais += fatorial(primos[i])
    }

    def media = somaFatoriais / primos.size()
    println("A média dos fatoriais dos números primos na lista é: " + media)
  }
}

// Exemplo de uso da classe ListaPrimos
def lista = new ListaPrimos()
lista.adicionarPrimo(2)
lista.adicionarPrimo(3)
lista.adicionarPrimo(5)
lista.adicionarPrimo(7)
lista.adicionarPrimo(11)

lista.somaPrimos()
lista.mediaFatoriais()
```

Explicação do código:

1. Começamos definindo uma função personalizada chamada `fatorial` que calcula o fatorial de um número usando recursão.
2. Em seguida, criamos uma classe chamada `ListaPrimos` para manipular listas de números primos.
3. Dentro da classe `ListaPrimos`, temos o método `adicionarPrimo` que adiciona um número primo à lista, verificando se ele realmente é primo antes de adicioná-lo.
4. O método `isPrimo` verifica se um número é primo, percorrendo todos os números de 2 até esse número e verificando se há algum divisor.
5. O método `somaPrimos` calcula a soma de todos os números primos na lista e a exibe na saída.
6. O método `mediaFatoriais` calcula a média dos fatoriais de todos os números primos na lista, somando os fatoriais e dividindo pelo número de primos na lista. O resultado é exibido na saída.
7. Por fim, temos um exemplo de uso da classe `ListaPrimos`, onde criamos uma instância, adicionamos alguns números primos à lista e chamamos os métodos `somaPrimos` e `mediaFatoriais` para exibir os resultados.

Espero que este código atenda às suas expectativas!