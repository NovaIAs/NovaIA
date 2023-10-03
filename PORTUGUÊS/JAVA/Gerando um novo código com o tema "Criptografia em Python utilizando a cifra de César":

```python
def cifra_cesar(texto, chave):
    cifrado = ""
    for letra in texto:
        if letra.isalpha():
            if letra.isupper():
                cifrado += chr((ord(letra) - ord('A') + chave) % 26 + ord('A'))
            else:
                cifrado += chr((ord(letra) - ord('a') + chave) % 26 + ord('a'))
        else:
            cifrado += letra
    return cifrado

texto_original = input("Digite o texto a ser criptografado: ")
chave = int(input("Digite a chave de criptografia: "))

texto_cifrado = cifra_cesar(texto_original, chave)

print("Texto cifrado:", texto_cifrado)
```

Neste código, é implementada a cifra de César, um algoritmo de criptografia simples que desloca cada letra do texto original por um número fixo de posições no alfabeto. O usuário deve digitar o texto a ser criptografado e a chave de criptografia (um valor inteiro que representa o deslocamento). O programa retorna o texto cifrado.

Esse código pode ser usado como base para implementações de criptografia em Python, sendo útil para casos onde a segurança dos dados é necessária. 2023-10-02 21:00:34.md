Claro! Aqui está um exemplo de um código complexo em Java que envolve manipulação de arrays e matrizes:

```java
import java.util.Scanner;

public class MatrizTransposta {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.print("Digite o número de linhas da matriz: ");
        int linhas = scanner.nextInt();
        System.out.print("Digite o número de colunas da matriz: ");
        int colunas = scanner.nextInt();

        int[][] matriz = new int[linhas][colunas];

        System.out.println("Digite os elementos da matriz:");

        // Receber os elementos da matriz
        for (int i = 0; i < linhas; i++) {
            for (int j = 0; j < colunas; j++) {
                matriz[i][j] = scanner.nextInt();
            }
        }

        int[][] matrizTransposta = new int[colunas][linhas];

        // Calcular a matriz transposta
        for (int i = 0; i < colunas; i++) {
            for (int j = 0; j < linhas; j++) {
                matrizTransposta[i][j] = matriz[j][i];
            }
        }

        System.out.println("A matriz transposta é:");

        // Imprimir a matriz transposta
        for (int i = 0; i < colunas; i++) {
            for (int j = 0; j < linhas; j++) {
                System.out.print(matrizTransposta[i][j] + " ");
            }
            System.out.println();
        }

        scanner.close();
    }
}
```

Neste código, o usuário deve informar o número de linhas e colunas da matriz. Em seguida, o usuário é solicitado a digitar os elementos da matriz, um por vez. O programa calcula a matriz transposta, ou seja, inverte as linhas pelas colunas da matriz original. Por fim, exibe a matriz transposta na saída.

Esse código é um exemplo de manipulação de matrizes e arrays bidimensionais em Java. Ele pode ser útil em casos onde é necessário realizar cálculos ou operações específicas com matrizes.