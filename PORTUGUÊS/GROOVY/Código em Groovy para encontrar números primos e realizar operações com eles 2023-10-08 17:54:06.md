Claro! Aqui está um código complexo em Groovy que realiza uma série de tarefas específicas:

```groovy
import java.util.stream.IntStream

class ComplexCode {
    static void main(String[] args) {
        // 1. Criando uma lista de números primos
        List<Integer> primeNumbers = IntStream.rangeClosed(2, 1000)
                .filter(ComplexCode::isPrime)
                .boxed()
                .collect(Collectors.toList())

        // 2. Imprimindo os números primos
        println("Números primos:")
        primeNumbers.each {
            println(it)
        }

        // 3. Calculando a soma dos números primos
        Integer sum = primeNumbers.sum()
        println("Soma dos números primos: ${sum}")

        // 4. Calculando a média dos números primos
        Double average = primeNumbers.average().orElse(0)
        println("Média dos números primos: ${average}")

        // 5. Encontrando o maior número primo
        Integer maxPrime = primeNumbers.max(Integer::compareTo).orElse(0)
        println("Maior número primo: ${maxPrime}")

        // 6. Encontrando o menor número primo
        Integer minPrime = primeNumbers.min(Integer::compareTo).orElse(0)
        println("Menor número primo: ${minPrime}")

        // 7. Verificando se um número específico é primo
        Integer numberToCheck = 37
        if (isPrime(numberToCheck)) {
            println("${numberToCheck} é um número primo.")
        } else {
            println("${numberToCheck} não é um número primo.")
        }
    }

    static boolean isPrime(int number) {
        if (number <= 1) {
            return false
        }

        for (int i = 2; i <= Math.sqrt(number); i++) {
            if (number % i == 0) {
                return false
            }
        }

        return true
    }
}
```

Explicação do código:

1. O código começa criando uma lista de números primos de 2 a 1000 usando a função `IntStream.rangeClosed()`, `filter()`, `boxed()` e `collect()`. A função `isPrime()` é usada como um predicado para filtrar apenas os números primos.

2. Em seguida, o código imprime todos os números primos encontrados.

3. O código calcula a soma dos números primos usando a função `sum()`.

4. O código calcula a média dos números primos usando a função `average()`.

5. O código encontra o maior número primo usando a função `max()`.

6. O código encontra o menor número primo usando a função `min()`.

7. O código verifica se um número específico (37 neste caso) é um número primo usando a função `isPrime()`.

Espero que isso lhe dê uma ideia de como criar um código complexo em Groovy.