Claro! Aqui está um código complexo em Java que envolve diversos conceitos e recursos da linguagem:

```java
import java.util.Scanner;

public class CalculadoraAvancada {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Bem-vindo(a) à Calculadora Avançada!");
        System.out.println("Digite a operação que deseja realizar (ex: 2 + 3):");
        String operacao = scanner.nextLine();
        
        // Verifica se a operação contém um dos operadores (+, -, *, /)
        if (operacao.contains("+") || operacao.contains("-") ||
            operacao.contains("*") || operacao.contains("/")) {
            
            // Remove espaços em branco e separa os operandos e o operador
            String[] partes = operacao.replaceAll("\\s", "").split("([+\\-*/])");
            double operando1 = Double.parseDouble(partes[0]);
            double operando2 = Double.parseDouble(partes[2]);
            char operador = partes[1].charAt(0);
            
            double resultado = 0;
            
            // Executa a operação solicitada
            switch (operador) {
                case '+':
                    resultado = operando1 + operando2;
                    break;
                case '-':
                    resultado = operando1 - operando2;
                    break;
                case '*':
                    resultado = operando1 * operando2;
                    break;
                case '/':
                    resultado = operando1 / operando2;
                    break;
            }
            
            System.out.println("O resultado da operação " + operacao + " é: " + resultado);
        } else {
            System.out.println("Operação inválida. Certifique-se de usar um dos operadores (+, -, *, /).");
        }
        
        scanner.close();
    }
}
```

Este código é uma calculadora avançada em Java que permite ao usuário realizar operações matemáticas básicas (+, -, *, /). Ele utiliza a classe `Scanner` para obter a entrada do usuário e processa a operação digitada.

Primeiro, o programa exibe uma mensagem de boas-vindas e solicita ao usuário que digite a operação que deseja realizar. A operação deve ser digitada no formato "operando1 operador operando2" (por exemplo, "2 + 3").

Em seguida, o programa verifica se a operação contém um dos operadores (+, -, *, /). Se não contiver, uma mensagem de erro é exibida. Caso contrário, o programa remove os espaços em branco da operação e separa os operandos e o operador em um array de strings.

Depois disso, o programa converte os operandos para números de ponto flutuante (`double`) e o operador para um caractere (`char`).

Em seguida, é inicializada uma variável `resultado` que irá armazenar o resultado da operação.

A operação é executada dentro de um switch-case, onde cada caso corresponde a um operador. Dependendo do operador, a operação correspondente é executada e o resultado é atribuído à variável `resultado`.

Por fim, o programa exibe o resultado da operação ao usuário.

Esse código é um exemplo de como criar uma calculadora básica em Java, utilizando recursos como entrada do usuário, manipulação de strings, conversão de tipos e estruturas de controle. Ele demonstra alguns conceitos fundamentais da linguagem Java e pode ser usado como ponto de partida para construir uma calculadora mais avançada.