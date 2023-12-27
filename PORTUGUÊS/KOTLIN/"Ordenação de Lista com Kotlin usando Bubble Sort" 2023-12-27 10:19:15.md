Claro! Aqui está um exemplo de código Kotlin complexo e diferenciado:

```kotlin
// Importando bibliotecas necessárias
import java.util.*

// Definindo uma classe principal
fun main() {
    // Criando um objeto Scanner para entrada de dados
    val scanner = Scanner(System.`in`)

    // Obtendo o número de elementos para a lista
    print("Digite o tamanho da lista: ")
    val tamanhoLista = scanner.nextInt()

    // Criando uma lista vazia
    val lista = mutableListOf<Int>()

    // Preenchendo a lista com números aleatórios
    for (i in 0 until tamanhoLista) {
        lista.add(Random().nextInt(100))
    }

    // Imprimindo a lista original
    println("Lista original: $lista")

    // Ordenando a lista em ordem crescente usando o algoritmo Bubble Sort
    for (i in 0 until tamanhoLista - 1) {
        for (j in 0 until tamanhoLista - i - 1) {
            if (lista[j] > lista[j + 1]) {
                val temp = lista[j]
                lista[j] = lista[j + 1]
                lista[j + 1] = temp
            }
        }
    }

    // Imprimindo a lista ordenada
    println("Lista ordenada: $lista")
}
```

Neste código, é solicitado ao usuário que digite o tamanho da lista. Em seguida, a lista é preenchida com números aleatórios usando a classe `Random`. A lista original é impressa e, em seguida, é realizada a ordenação da lista usando o algoritmo Bubble Sort. Por fim, a lista ordenada é impressa na tela. 

O algoritmo Bubble Sort percorre a lista várias vezes, comparando pares de elementos adjacentes e trocando-os caso estejam na ordem errada. Esse processo é repetido até que a lista esteja totalmente ordenada.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, é só pedir.