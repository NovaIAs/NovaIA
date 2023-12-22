Claro! Aqui está um exemplo de código em Java que realiza um algoritmo de busca binária em um array de números inteiros. O código é complexo e possui comentários explicativos em português para facilitar a compreensão:

```java
import java.util.Arrays;

public class BuscaBinaria {
    public static void main(String[] args) {
        int[] numeros = {2, 4, 6, 8, 10, 12, 14, 16, 18, 20}; // Array de números ordenados
        int elemento = 12; // Elemento a ser buscado
        
        int indice = buscaBinaria(numeros, elemento); // Chamada do método de busca binária
        
        if (indice != -1) {
            System.out.println("O elemento " + elemento + " foi encontrado no índice " + indice);
        } else {
            System.out.println("O elemento " + elemento + " não foi encontrado");
        }
    }
    
    public static int buscaBinaria(int[] array, int elemento) {
        int inicio = 0;
        int fim = array.length - 1;
        
        while (inicio <= fim) {
            int meio = (inicio + fim) / 2;
            
            if (array[meio] == elemento) {
                return meio; // Elemento encontrado, retorna o índice
            } else if (array[meio] < elemento) {
                inicio = meio + 1; // Elemento está na metade direita do array
            } else {
                fim = meio - 1; // Elemento está na metade esquerda do array
            }
        }
        
        return -1; // Elemento não encontrado
    }
}
```

Explicação do código:

1. Começamos definindo um array de números inteiros ordenados `numeros` e o elemento que desejamos buscar `elemento`.
2. Em seguida, chamamos o método `buscaBinaria` passando o array e o elemento como argumentos.
3. O método `buscaBinaria` recebe o array e o elemento a ser buscado como parâmetros.
4. Inicializamos as variáveis `inicio` e `fim` que representam as extremidades do array.
5. Utilizamos um loop while para realizar a busca binária.
6. Calculamos o índice do elemento do meio do array utilizando a fórmula `(inicio + fim) / 2`.
7. Comparamos o elemento do meio com o elemento que estamos buscando.
8. Se forem iguais, retornamos o índice do elemento encontrado.
9. Caso contrário, se o elemento do meio for menor que o elemento buscado, atualizamos o valor de `inicio` para `meio + 1`, pois o elemento está na metade direita do array.
10. Caso contrário, atualizamos o valor de `fim` para `meio - 1`, pois o elemento está na metade esquerda do array.
11. Se o elemento não for encontrado após percorrer todo o array, retornamos -1.
12. No método `main`, verificamos se o valor retornado do método `buscaBinaria` é diferente de -1.
13. Se for diferente de -1, exibimos uma mensagem informando o índice do elemento encontrado.
14. Caso contrário, exibimos uma mensagem informando que o elemento não foi encontrado.

Esse código implementa um algoritmo de busca binária eficiente que possui uma complexidade de tempo de O(log n), onde n é o tamanho do array.