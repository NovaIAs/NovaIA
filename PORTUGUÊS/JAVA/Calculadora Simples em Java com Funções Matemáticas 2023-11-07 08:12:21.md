Claro! Aqui está um exemplo de um código complexo em Java, que utiliza várias funcionalidades e é explicado passo a passo:

```java
import java.util.Scanner;

public class Calculadora {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Bem-vindo à Calculadora!");
        System.out.println("Selecione a operação que deseja realizar:");
        System.out.println("1. Soma");
        System.out.println("2. Subtração");
        System.out.println("3. Multiplicação");
        System.out.println("4. Divisão");

        int operacao = scanner.nextInt();

        System.out.println("Digite o primeiro número:");
        double numero1 = scanner.nextDouble();

        System.out.println("Digite o segundo número:");
        double numero2 = scanner.nextDouble();

        double resultado = 0;

        if (operacao == 1) {
            resultado = somar(numero1, numero2);
        } else if (operacao == 2) {
            resultado = subtrair(numero1, numero2);
        } else if (operacao == 3) {
            resultado = multiplicar(numero1, numero2);
        } else if (operacao == 4) {
            resultado = dividir(numero1, numero2);
        } else {
            System.out.println("Operação inválida!");
            System.exit(0); // Encerra o programa caso a operação seja inválida
        }

        System.out.println("O resultado da operação é: " + resultado);

        scanner.close();
    }

    // Função para realizar a soma de dois números
    public static double somar(double a, double b) {
        return a + b;
    }

    // Função para realizar a subtração de dois números
    public static double subtrair(double a, double b) {
        return a - b;
    }

    // Função para realizar a multiplicação de dois números
    public static double multiplicar(double a, double b) {
        return a * b;
    }

    // Função para realizar a divisão de dois números
    public static double dividir(double a, double b) {
        if (b == 0) {
            System.out.println("Não é possível dividir por zero!");
            System.exit(0); // Encerra o programa caso ocorra uma divisão por zero
        }
        return a / b;
    }
}
```

Neste exemplo, criamos uma calculadora simples em Java que permite ao usuário realizar operações matemáticas básicas, como soma, subtração, multiplicação e divisão. O programa começa exibindo um menu de opções para o usuário selecionar a operação desejada.

Em seguida, o programa solicita ao usuário que digite os dois números para a operação selecionada. Com base na escolha do usuário, a calculadora chama a função correspondente para realizar a operação desejada.

Por fim, o resultado da operação é exibido na tela. Vale ressaltar que o código também lida com casos de divisão por zero, exibindo uma mensagem de erro e encerrando o programa nesta situação.

Esse é apenas um exemplo de código complexo em Java. A complexidade de um código pode variar dependendo dos requisitos e funcionalidades necessárias para determinada aplicação.