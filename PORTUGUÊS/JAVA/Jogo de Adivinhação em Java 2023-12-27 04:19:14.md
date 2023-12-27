Certamente! Aqui está um exemplo de um programa em Java que usa uma abordagem modular para criar um jogo de adivinhação de números:

```java
import java.util.Random;
import java.util.Scanner;

public class JogoAdivinhacao {
    private static final int MAX_TENTATIVAS = 5;
    private static final int LIMITE_SUP = 100;

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Random random = new Random();

        int numeroAleatorio = random.nextInt(LIMITE_SUP) + 1;
        int tentativasRestantes = MAX_TENTATIVAS;

        System.out.println("Bem-vindo ao jogo de adivinhação!");
        System.out.println("Tente adivinhar um número de 1 a " + LIMITE_SUP);

        while (tentativasRestantes > 0) {
            System.out.println("Tentativas restantes: " + tentativasRestantes);
            System.out.print("Digite um número: ");
            int palpite = scanner.nextInt();

            if (palpite == numeroAleatorio) {
                System.out.println("Parabéns! Você acertou!");
                return;
            } else if (palpite < numeroAleatorio) {
                System.out.println("O número é maior!");
            } else {
                System.out.println("O número é menor!");
            }

            tentativasRestantes--;
        }

        System.out.println("Você perdeu! O número era " + numeroAleatorio);
    }
}
```

Neste código, um número aleatório é gerado entre 1 e 100 usando a classe `Random`. O jogador tem um número limitado de tentativas para adivinhar o número correto. A cada tentativa, o programa verifica se o palpite é igual ao número gerado e dá feedback se o palpite é maior ou menor. Se o jogador esgotar todas as tentativas sem acertar o número, o programa informa qual era o número correto.

Esta é apenas uma implementação básica de um jogo de adivinhação em Java, mas existem muitas maneiras de expandir e aprimorar este código.