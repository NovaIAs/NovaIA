Claro! Aqui está um código complexo em Groovy que utiliza vários conceitos e recursos avançados da linguagem:

```groovy
import java.util.concurrent.*

class ThreadedWorker implements Callable<Integer> {
    int start
    int end

    ThreadedWorker(int start, int end) {
        this.start = start
        this.end = end
    }

    @Override
    Integer call() throws Exception {
        int sum = 0
        for (int i = start; i <= end; i++) {
            sum += i
            Thread.sleep(10) // Simula um processamento demorado
        }
        return sum
    }
}

def calculateSumInParallel() {
    int start = 1
    int end = 1000
    int numThreads = 4

    ExecutorService executor = Executors.newFixedThreadPool(numThreads)
    List<Future<Integer>> futures = []
    int step = (end - start + 1) / numThreads

    for (int i = 0; i < numThreads; i++) {
        int threadStart = start + (i * step)
        int threadEnd = threadStart + step - 1

        if (i == numThreads - 1) {
            threadEnd = end // A última thread lida com possíveis sobras
        }

        Callable<Integer> worker = new ThreadedWorker(threadStart, threadEnd)
        Future<Integer> future = executor.submit(worker)
        futures.add(future)
    }

    int sum = 0
    for (Future<Integer> future : futures) {
        sum += future.get()
    }

    executor.shutdown()
    return sum
}

def main() {
    int sum = calculateSumInParallel()
    println("A soma dos números de 1 a 1000 calculada em paralelo é: $sum")
}

main()
```

Neste código, criamos uma classe `ThreadedWorker` que implementa a interface `Callable` do Java, permitindo que seja executada em uma thread separada. Essa classe realiza um processamento demorado simulado, onde cada instância realiza a soma de um intervalo de números.

A função `calculateSumInParallel` é responsável por dividir o problema em várias threads, cada uma somando uma parte dos números de 1 a 1000. Utilizamos um `ExecutorService` para gerenciar as threads e uma lista de `Future` para armazenar os resultados parciais.

A função `main` chama `calculateSumInParallel` para obter a soma total e imprime o resultado na tela.

Esse código utiliza programação concorrente para acelerar o cálculo da soma dos números de 1 a 1000, dividindo o trabalho em várias threads. Cada thread é responsável por calcular a soma de uma parte do intervalo, e os resultados parciais são somados no final.