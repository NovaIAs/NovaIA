Claro! Abaixo segue um exemplo de código complexo em Java que utiliza recursividade para encontrar a soma de todos os números de 1 até um determinado número inserido pelo usuário.

```java
import java.util.Scanner;

public class SomaRecursiva {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Digite um número inteiro positivo: ");
        int numero = scanner.nextInt();
        scanner.close();

        int soma = calcularSomaRecursiva(numero);

        System.out.println("A soma dos números de 1 até " + numero + " é: " + soma);
    }

    public static int calcularSomaRecursiva(int n) {
        if (n == 1) {
            return 1;
        } else {
            return n + calcularSomaRecursiva(n - 1);
        }
    }
}
```

Explicação do código:
1. A classe `SomaRecursiva` contém o método `main`, que é o ponto de entrada do programa.
2. Dentro do método `main`, é criado um objeto `Scanner` para receber a entrada do usuário.
3. O usuário é solicitado a inserir um número inteiro positivo, que é armazenado na variável `numero`.
4. O objeto `Scanner` é fechado para liberar os recursos.
5. O método `calcularSomaRecursiva` é chamado, passando como argumento o número digitado pelo usuário. O resultado dessa chamada é armazenado na variável `soma`.
6. Por fim, é exibida a mensagem com o resultado da soma.

No método `calcularSomaRecursiva`:
1. A função verifica se `n` é igual a 1. Caso seja, retorna 1 como base da recursão.
2. Caso contrário, utiliza o conceito de recursividade para chamar novamente o próprio método, passando `n - 1` como argumento, e soma o resultado com `n`. Esse processo é repetido até que `n` seja igual a 1, onde a recursão é encerrada.

Assim, o código utiliza recursividade para calcular a soma dos números de 1 até um valor determinado pelo usuário de forma eficiente e eficaz.