Claro! Aqui está um código complexo em Java que pode ser usado para calcular a soma de todos os números primos em um intervalo específico:

```java
import java.util.Scanner;

public class SomaNumerosPrimos {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Digite o valor inicial do intervalo: ");
        int inicio = scanner.nextInt();
        
        System.out.println("Digite o valor final do intervalo: ");
        int fim = scanner.nextInt();
        
        int soma = 0;
        for (int i = inicio; i <= fim; i++) {
            if (isPrimo(i)) {
                soma += i;
            }
        }
        
        System.out.println("A soma dos números primos no intervalo de " + inicio + " a " + fim + " é: " + soma);
    }
    
    // Verifica se um número é primo
    private static boolean isPrimo(int numero) {
        if (numero <= 1) {
            return false;
        }
        
        for (int i = 2; i <= Math.sqrt(numero); i++) {
            if (numero % i == 0) {
                return false;
            }
        }
        
        return true;
    }
}
```

Este código solicitará ao usuário que insira um valor inicial e um valor final para um intervalo específico. Em seguida, ele calculará a soma de todos os números primos dentro desse intervalo, utilizando a função `isPrimo()` para verificar se cada número é primo.

A função `isPrimo()` verifica se um número é primo ou não, retornando `true` se for primo e `false` caso contrário. Ela utiliza um loop para verificar se o número é divisível por qualquer outro número entre 2 e a raiz quadrada do número em questão. Caso seja divisível, o número não é primo.

Após o cálculo da soma, o código exibe a mensagem com o resultado da soma dos números primos no intervalo fornecido pelo usuário.

Espero que este código atenda às suas expectativas!