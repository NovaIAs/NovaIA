Claro! Aqui está um exemplo de código complexo em Java:

```java
import java.util.Scanner;

public class CalculadoraAvancada {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Bem-vindo à Calculadora Avançada!");
        System.out.println("Por favor, selecione a operação que deseja realizar:");
        System.out.println("1 - Soma");
        System.out.println("2 - Subtração");
        System.out.println("3 - Multiplicação");
        System.out.println("4 - Divisão");
        System.out.println("5 - Potência");
        System.out.println("6 - Raiz Quadrada");
        System.out.println("7 - Fatorial");
        
        int operacao = scanner.nextInt();
        
        switch (operacao) {
            case 1:
                realizarSoma();
                break;
            case 2:
                realizarSubtracao();
                break;
            case 3:
                realizarMultiplicacao();
                break;
            case 4:
                realizarDivisao();
                break;
            case 5:
                realizarPotencia();
                break;
            case 6:
                realizarRaizQuadrada();
                break;
            case 7:
                realizarFatorial();
                break;
            default:
                System.out.println("Operação inválida.");
                break;
        }
        
        scanner.close();
    }
    
    public static void realizarSoma() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Digite o primeiro número:");
        double numero1 = scanner.nextDouble();
        
        System.out.println("Digite o segundo número:");
        double numero2 = scanner.nextDouble();
        
        double resultado = numero1 + numero2;
        
        System.out.println("O resultado da soma é: " + resultado);
        
        scanner.close();
    }
    
    public static void realizarSubtracao() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Digite o primeiro número:");
        double numero1 = scanner.nextDouble();
        
        System.out.println("Digite o segundo número:");
        double numero2 = scanner.nextDouble();
        
        double resultado = numero1 - numero2;
        
        System.out.println("O resultado da subtração é: " + resultado);
        
        scanner.close();
    }
    
    public static void realizarMultiplicacao() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Digite o primeiro número:");
        double numero1 = scanner.nextDouble();
        
        System.out.println("Digite o segundo número:");
        double numero2 = scanner.nextDouble();
        
        double resultado = numero1 * numero2;
        
        System.out.println("O resultado da multiplicação é: " + resultado);
        
        scanner.close();
    }
    
    public static void realizarDivisao() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Digite o primeiro número:");
        double numero1 = scanner.nextDouble();
        
        System.out.println("Digite o segundo número:");
        double numero2 = scanner.nextDouble();
        
        if (numero2 == 0) {
            System.out.println("Não é possível dividir por zero.");
        } else {
            double resultado = numero1 / numero2;
            System.out.println("O resultado da divisão é: " + resultado);
        }
        
        scanner.close();
    }
    
    public static void realizarPotencia() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Digite a base:");
        double base = scanner.nextDouble();
        
        System.out.println("Digite o expoente:");
        double expoente = scanner.nextDouble();
        
        double resultado = Math.pow(base, expoente);
        
        System.out.println("O resultado da potência é: " + resultado);
        
        scanner.close();
    }
    
    public static void realizarRaizQuadrada() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Digite um número:");
        double numero = scanner.nextDouble();
        
        if (numero < 0) {
            System.out.println("Não é possível calcular a raiz quadrada de um número negativo.");
        } else {
            double resultado = Math.sqrt(numero);
            System.out.println("O resultado da raiz quadrada é: " + resultado);
        }
        
        scanner.close();
    }
    
    public static void realizarFatorial() {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Digite um número inteiro não negativo:");
        int numero = scanner.nextInt();
        
        if (numero < 0) {
            System.out.println("Não é possível calcular o fatorial de um número negativo.");
        } else {
            int resultado = 1;
            
            for (int i = 1; i <= numero; i++) {
                resultado *= i;
            }
            
            System.out.println("O resultado do fatorial é: " + resultado);
        }
        
        scanner.close();
    }
}
```

Este código é uma calculadora avançada em Java, onde o usuário pode selecionar diferentes operações matemáticas para realizar. A calculadora oferece as seguintes operações: soma, subtração, multiplicação, divisão, potência, raiz quadrada e fatorial.

O código começa solicitando ao usuário que selecione a operação desejada. Em seguida, um switch-case é usado para chamar o método correspondente à operação selecionada.

Cada método de operação solicita os números necessários para realizar a operação e exibe o resultado correspondente.

O código também inclui verificações para garantir que operações inválidas, como divisão por zero ou cálculo de raiz quadrada de um número negativo, não sejam realizadas.

Ao finalizar a execução de cada método, o código fecha o scanner para evitar vazamentos de recursos.

Espero que este código complexo em Java seja útil para você! Se tiver alguma dúvida ou precisar de mais ajuda, fique à vontade para perguntar.