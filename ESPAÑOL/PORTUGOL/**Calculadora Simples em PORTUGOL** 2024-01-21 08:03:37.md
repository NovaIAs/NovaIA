```portuguol
programa Calculadora {
  # Esta linha é a função main do programa
  função main() {
    # Declaração de variáveis
    var numero1, numero2, resultado: real;
    var operacao: caractere;

    # Leitura dos valores
    escreva("Digite o primeiro número: ");
    leia(numero1);
    escreva("Digite o segundo número: ");
    leia(numero2);
    escreva("Digite a operação desejada (+, -, *, /): ");
    leia(operacao);

    # Realiza a operação desejada
    de acordo com operacao {
      caso '+':
        resultado := numero1 + numero2;
      caso '-':
        resultado := numero1 - numero2;
      caso '*':
        resultado := numero1 * numero2;
      caso '/':
        resultado := numero1 / numero2;
      caso outro:
        escreva("Operação inválida!");
        retorna;
    }

    # Exibir o resultado
    escreva("O resultado é: ", resultado);
  }
}
```

Este código é uma calculadora simples escrita em PORTUGOL. Ele permite que o usuário digite dois números e uma operação (+, -, *, /) e, em seguida, exibe o resultado da operação.

O código começa com a declaração do programa `Calculadora` e da função `main`, que é a função principal do programa.

Em seguida, são declaradas as variáveis `numero1`, `numero2`, `resultado` e `operacao`. As variáveis `numero1` e `numero2` são usadas para armazenar os dois números que o usuário digita, a variável `resultado` é usada para armazenar o resultado da operação e a variável `operacao` é usada para armazenar a operação que o usuário deseja realizar.

Em seguida, o programa lê os valores das variáveis `numero1`, `numero2` e `operacao` do usuário.

O programa então usa a instrução `de acordo com` para realizar a operação desejada. A instrução `de acordo com` funciona da seguinte forma:

* Ela avalia a variável `operacao` e compara-a com os valores dos casos.
* Se o valor da variável `operacao` for igual a um dos valores dos casos, o bloco de código associado a esse caso é executado.
* Se o valor da variável `operacao` não for igual a nenhum dos valores dos casos, o bloco de código associado ao caso `outro` é executado.

No caso deste código, se o usuário digitar o caractere `+`, o bloco de código associado ao caso `+` é executado, que calcula a soma dos dois números digitados pelo usuário e armazena o resultado na variável `resultado`.

Se o usuário digitar o caractere `-`, o bloco de código associado ao caso `-` é executado, que calcula a diferença dos dois números digitados pelo usuário e armazena o resultado na variável `resultado`.

Se o usuário digitar o caractere `*`, o bloco de código associado ao caso `*` é executado, que calcula o produto dos dois números digitados pelo usuário e armazena o resultado na variável `resultado`.

Se o usuário digitar o caractere `/`, o bloco de código associado ao caso `/` é executado, que calcula o quociente dos dois números digitados pelo usuário e armazena o resultado na variável `resultado`.

Se o usuário digitar qualquer outro caractere, o bloco de código associado ao caso `outro` é executado, que exibe uma mensagem de erro.

Depois de realizar a operação desejada, o programa exibe o resultado da operação na tela.