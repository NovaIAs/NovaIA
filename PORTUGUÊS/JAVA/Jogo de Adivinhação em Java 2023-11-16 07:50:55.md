Claro! Aqui está um código em Java que simula um jogo de adivinhação de números. O objetivo do jogo é adivinhar o número gerado aleatoriamente pelo programa. Vou explicar o código passo a passo:

```java
import java.util.Scanner;

public class JogoAdivinhacao {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int numeroAleatorio = gerarNumeroAleatorio();
        int tentativas = 0;
        boolean acertou = false;

        System.out.println("Bem-vindo(a) ao jogo de adivinhação de números!");
        System.out.println("Tente adivinhar o número entre 1 e 100.");

        while (!acertou) {
            System.out.print("Digite um número: ");
            int palpite = scanner.nextInt();
            tentativas++;

            if (palpite < numeroAleatorio) {
                System.out.println("O número digitado é menor que o número a ser adivinhado.");
            } else if (palpite > numeroAleatorio) {
                System.out.println("O número digitado é maior que o número a ser adivinhado.");
            } else {
                acertou = true;
            }
        }

        System.out.println("Parabéns! Você acertou o número em " + tentativas + " tentativas.");
        scanner.close();
    }

    public static int gerarNumeroAleatorio() {
        return (int) (Math.random() * 100) + 1; // Gera um número aleatório entre 1 e 100
    }
}
```

Explicação do código:

1. Importamos a classe `Scanner` do pacote `java.util` para ler os números digitados pelo usuário.
2. Criamos a classe `JogoAdivinhacao` que contém o método `main`.
3. Dentro do método `main`, criamos um objeto `Scanner` para ler os valores digitados pelo usuário e declaramos as variáveis `numeroAleatorio`, `tentativas` e `acertou`.
4. Imprimimos as instruções do jogo na tela.
5. Iniciamos um loop `while` que irá repetir até que o jogador acerte o número.
6. Dentro do loop, pedimos para o jogador digitar um número e armazenamos o valor em `palpite`.
7. Incrementamos o número de `tentativas` em cada iteração.
8. Comparamos o `palpite` com o `numeroAleatorio` para decidir se o número é maior, menor ou se o jogador acertou.
9. Se o `palpite` for menor que o `numeroAleatorio`, imprimimos uma mensagem informando ao jogador que o número é menor.
10. Se o `palpite` for maior que o `numeroAleatorio`, imprimimos uma mensagem informando ao jogador que o número é maior.
11. Se o `palpite` for igual ao `numeroAleatorio`, atribuímos `true` à variável `acertou` para sair do loop.
12. Depois que o jogador acertar o número, exibimos uma mensagem parabenizando-o e informamos o número de `tentativas` que ele levou para acertar.
13. Fechamos o objeto `Scanner`.
14. Definimos o método `gerarNumeroAleatorio` que retorna um número aleatório entre 1 e 100, utilizando o método `Math.random()` para gerar um número aleatório entre 0 (inclusive) e 1 (exclusivo) e multiplicando por 100 para obter um número entre 0 (inclusive) e 100 (exclusivo). Em seguida, adicionamos 1 para garantir que o número esteja entre 1 e 100.