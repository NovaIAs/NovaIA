Claro! Aqui está um exemplo de código complexo em Kotlin:

```kotlin
fun main() {
    val listaNumeros = mutableListOf<Int>()
    
    for (i in 1..100) {
        if (i % 2 == 0) {
            listaNumeros.add(i)
        }
    }
    
    val somaPares = listaNumeros.sum()
    
    println("A soma dos números pares de 1 a 100 é: $somaPares")
    
    val listaFatorial = mutableListOf<Long>()
    
    for (i in 1..10) {
        var fatorial: Long = 1
        
        for (j in 1..i) {
            fatorial *= j
        }
        
        listaFatorial.add(fatorial)
    }
    
    println("Os fatoriais dos números de 1 a 10 são:")
    
    for (i in 1..10) {
        println("$i! = ${listaFatorial[i-1]}")
    }
}
```

Neste código, começamos criando uma lista vazia chamada `listaNumeros` usando a função `mutableListOf()`. Em seguida, usamos um loop `for` para iterar de 1 a 100. Dentro do loop, verificamos se o número é par usando a condição `if (i % 2 == 0)`. Se for, adicionamos o número à lista utilizando o método `add()`.

Depois, calculamos a soma dos números pares da lista usando o método `sum()` e armazenamos o resultado na variável `somaPares`.

Em seguida, criamos outra lista vazia chamada `listaFatorial` para armazenar os fatoriais dos números de 1 a 10. Usamos outro loop `for` para iterar de 1 a 10. Dentro desse loop, inicializamos uma variável `fatorial` como 1 e usamos outro loop `for` para calcular o fatorial de cada número. Multiplicamos `fatorial` pelo valor de `j` em cada iteração.

Após calcular o fatorial, adicionamos o resultado à lista `listaFatorial` usando o método `add()`.

Por fim, imprimimos a soma dos números pares e os fatoriais dos números de 1 a 10 utilizando loops `for` e a função `println()`.

Espero que este código complexo em Kotlin atenda às suas expectativas!