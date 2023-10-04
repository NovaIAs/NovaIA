Claro! Aqui está um exemplo de um código complexo em Kotlin:

```kotlin
fun main() {
    val listaNumeros = mutableListOf<Int>() // Cria uma lista vazia para armazenar números
    var soma = 0 // Variável para guardar a soma dos números
    var media = 0.0 // Variável para guardar a média dos números

    for (i in 1..10) {
        val numero = lerNumero() // Chama a função lerNumero() para obter um número digitado pelo usuário
        listaNumeros.add(numero) // Adiciona o número na lista

        soma += numero // Incrementa a soma com o número digitado
    }

    media = soma.toDouble() / listaNumeros.size // Calcula a média dos números

    println("Números digitados: $listaNumeros")
    println("Soma: $soma")
    println("Média: $media")
}

fun lerNumero(): Int {
    print("Digite um número: ")
    val numero = readLine()?.toIntOrNull()

    if (numero == null) {
        println("Número inválido! Digite novamente.")
        return lerNumero() // Chama a função recursivamente, caso o número seja inválido
    }

    return numero
}
```

Neste código, criamos uma função `main` que será executada quando o programa for iniciado. O programa irá solicitar ao usuário que digite 10 números, que serão armazenados em uma lista chamada `listaNumeros`. Em seguida, os números da lista serão somados e a média será calculada. Por fim, os números digitados, a soma e a média serão exibidos na tela.

Também definimos a função `lerNumero`, que é responsável por ler um número digitado pelo usuário. Caso o número seja inválido (ou seja, não seja um valor numérico), a função irá exibir uma mensagem de erro e solicitará ao usuário que digite novamente. Essa função é chamada de forma recursiva até que um número válido seja fornecido.

Espero que este código atenda às suas expectativas!