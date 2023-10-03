Claro! Aqui está um exemplo de código em Java que implementa um algoritmo de ordenação chamado Merge Sort:

```java
public class MergeSort {
    public static void main(String[] args) {
        int[] array = {9, 5, 1, 3, 7, 2, 8, 6, 4};
        mergeSort(array, 0, array.length - 1);

        System.out.println("Array ordenado:");
        for (int element : array) {
            System.out.print(element + " ");
        }
    }

    public static void mergeSort(int[] array, int left, int right) {
        if (left < right) {
            int middle = (left + right) / 2;

            mergeSort(array, left, middle);
            mergeSort(array, middle + 1, right);

            merge(array, left, middle, right);
        }
    }

    public static void merge(int[] array, int left, int middle, int right) {
        int n1 = middle - left + 1;
        int n2 = right - middle;

        int[] leftArray = new int[n1];
        int[] rightArray = new int[n2];

        for (int i = 0; i < n1; i++) {
            leftArray[i] = array[left + i];
        }
        for (int i = 0; i < n2; i++) {
            rightArray[i] = array[middle + 1 + i];
        }

        int i = 0, j = 0;
        int k = left;

        while (i < n1 && j < n2) {
            if (leftArray[i] <= rightArray[j]) {
                array[k] = leftArray[i];
                i++;
            } else {
                array[k] = rightArray[j];
                j++;
            }
            k++;
        }

        while (i < n1) {
            array[k] = leftArray[i];
            i++;
            k++;
        }

        while (j < n2) {
            array[k] = rightArray[j];
            j++;
            k++;
        }
    }
}
```

O Merge Sort é um algoritmo de ordenação eficiente que utiliza a técnica de dividir para conquistar. Ele divide a lista em sublistas menores, ordena cada sublista e, em seguida, mescla as sublistas ordenadas para obter o resultado final.

Neste exemplo, o código começa definindo um array de números inteiros desordenados. Em seguida, chama a função `mergeSort` passando o array, o índice inicial (0) e o índice final (tamanho do array - 1).

A função `mergeSort` implementa a lógica do algoritmo Merge Sort. Ela verifica se o índice inicial é menor que o índice final, o que indica que ainda há elementos para serem ordenados. Se sim, calcula o índice do meio do array, chama recursivamente `mergeSort` para ordenar as duas metades e, por fim, chama a função `merge` para mesclar as metades ordenadas.

A função `merge` recebe como parâmetros o array, os índices inicial, meio e final. Ela cria dois arrays auxiliares para armazenar as sublistas esquerda e direita. Em seguida, copia os elementos correspondentes para cada array auxiliar.

A partir daí, a função realiza a mescla dos dois arrays auxiliares de forma ordenada. Ela compara os elementos de ambos os arrays e os insere de volta no array original, mantendo a ordem crescente. Por fim, os elementos restantes de cada array auxiliar são inseridos no array original, garantindo que todos os elementos sejam ordenados corretamente.

Após a ordenação, o código imprime o array ordenado na saída padrão.