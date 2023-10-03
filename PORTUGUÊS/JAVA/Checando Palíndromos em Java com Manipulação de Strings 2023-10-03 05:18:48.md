Claro! Aqui está um exemplo de código complexo em Java que envolve manipulação de strings e estruturas de controle:

```java
import java.util.Scanner;

public class VerificadorPalindromo {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Digite uma palavra ou frase: ");
        String entrada = scanner.nextLine();

        String textoFormatado = formatarTexto(entrada);
        boolean ehPalindromo = verificarPalindromo(textoFormatado);

        if (ehPalindromo) {
            System.out.println("A palavra/frase é um palíndromo!");
        } else {
            System.out.println("A palavra/frase não é um palíndromo.");
        }
    }

    public static String formatarTexto(String texto) {
        texto = texto.toLowerCase();
        texto = texto.replaceAll("[^a-z0-9]", "");
        return texto;
    }

    public static boolean verificarPalindromo(String texto) {
        int tamanho = texto.length();
        for (int i = 0; i < tamanho / 2; i++) {
            if (texto.charAt(i) != texto.charAt(tamanho - i - 1)) {
                return false;
            }
        }
        return true;
    }
}
```

Este código é um verificador de palíndromos em Java. Um palíndromo é uma palavra, frase, número ou qualquer sequência de caracteres que pode ser lida tanto da esquerda para a direita quanto da direita para a esquerda, mantendo o mesmo sentido e sem considerar espaços em branco ou pontuações. 

O programa solicita ao usuário que digite uma palavra ou frase e, em seguida, formata o texto removendo espaços em branco, pontuações e convertendo todas as letras para minúsculas. Isso é feito pela função `formatarTexto`.

Após a formatação, a função `verificarPalindromo` é chamada para verificar se o texto é um palíndromo. Essa função compara os caracteres no início e no fim da palavra de forma simultânea, avançando em direção ao centro. Se em algum momento os caracteres forem diferentes, a função retorna `false`, indicando que o texto não é um palíndromo. Caso contrário, retorna `true`.

Por fim, o programa exibe uma mensagem informando se a palavra/frase digitada é um palíndromo ou não, com base no resultado retornado pela função `verificarPalindromo`.