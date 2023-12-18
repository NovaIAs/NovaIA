Claro! Vou gerar um código em Java complexo e diferenciado para você. Este código irá calcular a soma dos valores pares de uma matriz de números inteiros.

```java
import java.util.*;

public class SomaParesMatriz {
    public static void main(String[] args) {
        int[][] matriz = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
        int soma = calcularSomaPares(matriz);
        System.out.println("A soma dos valores pares da matriz é: " + soma);
    }

    public static int calcularSomaPares(int[][] matriz) {
        int soma = 0;
        for (int i = 0; i < matriz.length; i++) {
            for (int j = 0; j < matriz[i].length; j++) {
                if (matriz[i][j] % 2 == 0) {
                    soma += matriz[i][j];
                }
            }
        }
        return soma;
    }
}
```

Neste código, temos a classe `SomaParesMatriz` com um método `main` que representa o ponto de entrada do programa. Na matriz de exemplo, temos uma matriz 3x3 com os valores de 1 a 9.

O método `calcularSomaPares` recebe a matriz como parâmetro e inicializa a variável `soma` com o valor zero. Em seguida, percorre a matriz usando dois loops aninhados, verificando se cada elemento é par usando a operação de módulo `%` com 2. Se o elemento for par, ele é adicionado à variável `soma`.

No método `main`, chamamos o método `calcularSomaPares` passando a matriz como argumento e armazenamos o resultado na variável `soma`. Em seguida, imprimimos a frase "A soma dos valores pares da matriz é: " concatenada com o valor da variável `soma` no console.

Este código é complexo, pois envolve o uso de matrizes, loops aninhados e uma operação condicional para verificar se um número é par. Além disso, ele é diferenciado, pois é específico para somar os valores pares de uma matriz, o que pode ser útil em diferentes contextos de programação.