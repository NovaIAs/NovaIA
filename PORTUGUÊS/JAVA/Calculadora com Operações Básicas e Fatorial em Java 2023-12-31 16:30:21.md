Claro, vou criar um código em Java que implementa uma calculadora com uma funcionalidade adicional. A calculadora terá as quatro operações básicas (adição, subtração, multiplicação e divisão) e também será capaz de calcular o fatorial de um número.

```java
import java.util.Scanner;

public class Calculadora {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Bem-vindo(a) à Calculadora!");
        System.out.println("Digite o número correspondente à operação que deseja realizar:");
        System.out.println("1 - Adição");
        System.out.println("2 - Subtração");
        System.out.println("3 - Multiplicação");
        System.out.println("4 - Divisão");
        System.out.println("5 - Fatorial");
        
        int operacao = scanner.nextInt();
        
        switch (operacao) {
            case 1:
                System.out.println("Digite o primeiro número:");
                double numero1 = scanner.nextDouble();
                System.out.println("Digite o segundo número:");
                double numero2 = scanner.nextDouble();
                double resultadoSoma = numero1 + numero2;
                System.out.println("O resultado da adição é: " + resultadoSoma);
                break;
                
            case 2:
                System.out.println("Digite o primeiro número:");
                double numero3 = scanner.nextDouble();
                System.out.println("Digite o segundo número:");
                double numero4 = scanner.nextDouble();
                double resultadoSubtracao = numero3 - numero4;
                System.out.println("O resultado da subtração é: " + resultadoSubtracao);
                break;
                
            case 3:
                System.out.println("Digite o primeiro número:");
                double numero5 = scanner.nextDouble();
                System.out.println("Digite o segundo número:");
                double numero6 = scanner.nextDouble();
                double resultadoMultiplicacao = numero5 * numero6;
                System.out.println("O resultado da multiplicação é: " + resultadoMultiplicacao);
                break;
                
            case 4:
                System.out.println("Digite o dividendo:");
                double dividendo = scanner.nextDouble();
                System.out.println("Digite o divisor:");
                double divisor = scanner.nextDouble();
                
                if (divisor == 0) {
                    System.out.println("Erro: divisão por zero não é permitida!");
                } else {
                    double resultadoDivisao = dividendo / divisor;
                    System.out.println("O resultado da divisão é: " + resultadoDivisao);
                }
                
                break;
                
            case 5:
                System.out.println("Digite um número inteiro positivo:");
                int numero = scanner.nextInt();
                
                if (numero < 0) {
                    System.out.println("Erro: o número deve ser positivo!");
                } else {
                    int fatorial = 1;
                    
                    for (int i = 1; i <= numero; i++) {
                        fatorial *= i;
                    }
                    
                    System.out.println("O fatorial de " + numero + " é: " + fatorial);
                }
                
                break;
                
            default:
                System.out.println("Erro: operação inválida!");
                break;
        }
        
        scanner.close();
    }
}
```

Neste código, criamos uma calculadora com a funcionalidade adicional de cálculo de fatorial. O programa exibe um menu com opções para o usuário escolher a operação desejada. Em seguida, o usuário insere os números necessários para a operação selecionada e o programa realiza o cálculo correspondente.

No caso da adição, subtração, multiplicação e divisão, o programa simplesmente realiza a operação e exibe o resultado. Para a divisão, é feita uma verificação para evitar a divisão por zero.

Para calcular o fatorial de um número, o usuário deve inserir um número inteiro positivo. O programa verifica se o número é válido e, em seguida, executa um laço de repetição para calcular o fatorial, multiplicando todos os números de 1 até o número inserido.

Espero que este código atenda às suas expectativas!