Claro! Aqui está um código em Java que cria uma lista de números aleatórios, ordena essa lista usando o algoritmo Bubble Sort e, em seguida, exibe o resultado ordenado:

```java
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class BubbleSort {
    public static void main(String[] args) {
        List<Integer> numeros = gerarNumerosAleatorios(10, 1, 100);
        System.out.println("Lista original: " + numeros);

        bubbleSort(numeros);

        System.out.println("Lista ordenada: " + numeros);
    }

    private static List<Integer> gerarNumerosAleatorios(int quantidade, int minimo, int maximo) {
        List<Integer> numeros = new ArrayList<>();
        Random random = new Random();

        for (int i = 0; i < quantidade; i++) {
            int numero = random.nextInt(maximo - minimo + 1) + minimo;
            numeros.add(numero);
        }

        return numeros;
    }

    private static void bubbleSort(List<Integer> numeros) {
        boolean trocou;
        int tamanho = numeros.size();

        do {
            trocou = false;

            for (int i = 0; i < tamanho - 1; i++) {
                if (numeros.get(i) > numeros.get(i + 1)) {
                    int temp = numeros.get(i);
                    numeros.set(i, numeros.get(i + 1));
                    numeros.set(i + 1, temp);
                    trocou = true;
                }
            }

            tamanho--;
        } while (trocou);
    }
}
```

Neste código, temos uma classe chamada `BubbleSort` que contém o método `main` como ponto de entrada do programa. Primeiro, chamamos o método `gerarNumerosAleatorios` para criar uma lista de 10 números aleatórios entre 1 e 100.

Em seguida, chamamos o método `bubbleSort` para ordenar a lista usando o algoritmo Bubble Sort. Este algoritmo compara pares de números adjacentes e os troca de posição se estiverem na ordem errada. Esse processo é repetido até que a lista esteja completamente ordenada.

Após a ordenação, exibimos a lista original e a lista ordenada usando `System.out.println`. O resultado será algo como:

```
Lista original: [54, 32, 87, 12, 78, 66, 99, 43, 29, 55]
Lista ordenada: [12, 29, 32, 43, 54, 55, 66, 78, 87, 99]
```

Espero que este código atenda às suas expectativas!