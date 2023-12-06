Claro, vou criar um código em Java que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo de ordenação eficiente que utiliza o conceito de dividir para conquistar.

```java
import java.util.Arrays;

public class MergeSort {
    public static void main(String[] args) {
        int[] array = {9, 5, 1, 3, 8, 4, 2, 7, 6};
        
        System.out.println("Array original: " + Arrays.toString(array));
        
        mergeSort(array, 0, array.length - 1);
        
        System.out.println("Array ordenado: " + Arrays.toString(array));
    }
    
    public static void mergeSort(int[] array, int inicio, int fim) {
        if (inicio < fim) {
            int meio = (inicio + fim) / 2;
            
            mergeSort(array, inicio, meio);
            mergeSort(array, meio + 1, fim);
            
            merge(array, inicio, meio, fim);
        }
    }
    
    public static void merge(int[] array, int inicio, int meio, int fim) {
        int n1 = meio - inicio + 1;
        int n2 = fim - meio;
        
        int[] esquerda = new int[n1];
        int[] direita = new int[n2];
        
        for (int i = 0; i < n1; i++) {
            esquerda[i] = array[inicio + i];
        }
        
        for (int j = 0; j < n2; j++) {
            direita[j] = array[meio + 1 + j];
        }
        
        int i = 0, j = 0;
        int k = inicio;
        
        while (i < n1 && j < n2) {
            if (esquerda[i] <= direita[j]) {
                array[k] = esquerda[i];
                i++;
            } else {
                array[k] = direita[j];
                j++;
            }
            k++;
        }
        
        while (i < n1) {
            array[k] = esquerda[i];
            i++;
            k++;
        }
        
        while (j < n2) {
            array[k] = direita[j];
            j++;
            k++;
        }
    }
}
```

Explicando o código:
1. Começamos definindo a classe `MergeSort`.
2. No método `main`, criamos um array com valores desordenados.
3. Imprimimos o array original.
4. Chamamos o método `mergeSort` para ordenar o array.
5. Por fim, imprimimos o array ordenado.
6. No método `mergeSort`, verificamos se a sublista a ser ordenada contém mais de um elemento.
7. Se sim, calculamos a posição do elemento do meio e chamamos recursivamente o `mergeSort` para as duas metades.
8. Em seguida, chamamos o método `merge` para mesclar as metades ordenadas.
9. No método `merge`, calculamos o tamanho das duas metades e criamos dois arrays temporários (esquerda e direita) para armazenar os elementos.
10. Copiamos os elementos da sublista esquerda e da sublista direita para os arrays temporários.
11. Em seguida, mesclamos os arrays temporários no array original de volta em ordem crescente.
12. No final, temos um array ordenado. 

Espero que esse código atenda às suas expectativas!