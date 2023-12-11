Claro! Aqui está um código complexo em Groovy, que realiza a manipulação de uma lista de números:

```groovy
// Definindo a classe principal
class ManipuladorLista {
    
    // Método principal
    static void main(String[] args) {
        List<Integer> numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] // Cria uma lista de números de 1 a 10
        
        println("Lista original: $numeros") // Imprime a lista original
        
        // Cria uma nova lista com os números pares da lista original
        List<Integer> numerosPares = numeros.findAll { it % 2 == 0 }
        println("Números pares: $numerosPares") // Imprime os números pares
        
        // Cria uma nova lista com os números ímpares da lista original
        List<Integer> numerosImpares = numeros.findAll { it % 2 != 0 }
        println("Números ímpares: $numerosImpares") // Imprime os números ímpares
        
        // Cria uma nova lista com o quadrado de cada número da lista original
        List<Integer> numerosQuadrado = numeros.collect { it * it }
        println("Quadrado de cada número: $numerosQuadrado") // Imprime o quadrado de cada número
        
        // Calcula a soma de todos os números da lista original
        Integer soma = numeros.inject(0) { acc, num -> acc + num }
        println("Soma dos números: $soma") // Imprime a soma
        
        // Ordena a lista original em ordem decrescente
        numeros.sort { b, a -> a <=> b }
        println("Lista ordenada em ordem decrescente: $numeros") // Imprime a lista ordenada
    }
}
```

Neste código, a classe `ManipuladorLista` possui um método `main` que é o ponto de entrada do programa. Nele, é criada uma lista de números de 1 a 10. Em seguida, são realizadas diversas manipulações nessa lista:

1. É criada uma nova lista chamada `numerosPares` que contém apenas os números pares da lista original.
2. É criada uma nova lista chamada `numerosImpares` que contém apenas os números ímpares da lista original.
3. É criada uma nova lista chamada `numerosQuadrado` que contém o quadrado de cada número da lista original.
4. É calculada a soma de todos os números da lista original.
5. É feita a ordenação da lista original em ordem decrescente.

Ao final de cada manipulação, os resultados são impressos no console.

Espero que este código atenda às suas expectativas! Lembre-se de que você pode personalizar e adaptar o código conforme necessário.