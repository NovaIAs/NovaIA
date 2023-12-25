Claro! Aqui está um exemplo de um código complexo em Java que envolve a manipulação de matrizes:

```java
import java.util.Scanner;

public class ManipulacaoMatrizes {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.print("Informe o número de linhas da matriz: ");
        int linhas = scanner.nextInt();

        System.out.print("Informe o número de colunas da matriz: ");
        int colunas = scanner.nextInt();

        int[][] matriz = new int[linhas][colunas];

        System.out.println("Preencha a matriz:");

        for (int i = 0; i < linhas; i++) {
            for (int j = 0; j < colunas; j++) {
                System.out.print("Informe o valor para a posição [" + i + "][" + j + "]: ");
                matriz[i][j] = scanner.nextInt();
            }
        }

        System.out.println("\nMatriz original:");
        imprimirMatriz(matriz);

        int[][] matrizTransposta = transporMatriz(matriz);
        System.out.println("\nMatriz transposta:");
        imprimirMatriz(matrizTransposta);

        int maiorValor = encontrarMaiorValor(matriz);
        System.out.println("\nO maior valor na matriz é: " + maiorValor);

        int somaDiagonalPrincipal = somarDiagonalPrincipal(matriz);
        System.out.println("A soma dos elementos da diagonal principal é: " + somaDiagonalPrincipal);

        scanner.close();
    }

    public static void imprimirMatriz(int[][] matriz) {
        for (int i = 0; i < matriz.length; i++) {
            for (int j = 0; j < matriz[0].length; j++) {
                System.out.print(matriz[i][j] + " ");
            }
            System.out.println();
        }
    }

    public static int[][] transporMatriz(int[][] matriz) {
        int[][] matrizTransposta = new int[matriz[0].length][matriz.length];
        for (int i = 0; i < matriz.length; i++) {
            for (int j = 0; j < matriz[0].length; j++) {
                matrizTransposta[j][i] = matriz[i][j];
            }
        }
        return matrizTransposta;
    }

    public static int encontrarMaiorValor(int[][] matriz) {
        int maiorValor = matriz[0][0];
        for (int i = 0; i < matriz.length; i++) {
            for (int j = 0; j < matriz[0].length; j++) {
                if (matriz[i][j] > maiorValor) {
                    maiorValor = matriz[i][j];
                }
            }
        }
        return maiorValor;
    }

    public static int somarDiagonalPrincipal(int[][] matriz) {
        int soma = 0;
        for (int i = 0; i < matriz.length; i++) {
            soma += matriz[i][i];
        }
        return soma;
    }
}
```

Este código permite ao usuário informar o número de linhas e colunas de uma matriz, preencher a matriz com valores, imprimir a matriz original, transpor a matriz, encontrar o maior valor na matriz e somar os elementos da diagonal principal.

A função `imprimirMatriz` é responsável por exibir a matriz na tela. A função `transporMatriz` realiza a transposição da matriz, ou seja, troca as linhas pelas colunas. A função `encontrarMaiorValor` busca e retorna o maior valor presente na matriz. E, por fim, a função `somarDiagonalPrincipal` calcula a soma dos elementos da diagonal principal da matriz.

Espero que esse código atenda às suas expectativas!