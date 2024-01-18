```portugol
PROGRAM TRIANGULO_DE_PASCAL;

VARIAVEIS
  linha, coluna, valor, resultado, potencia : INTEIRO;
  cont : INTEIRO;
  acumulador : INTEIRO;

INICIO
  escreva("Digite o número de linhas do triângulo de Pascal: ");
  leia(linha);

  escreva("\n");
  escreva("O triângulo de Pascal com ", linha, " linhas é:\n");

  para linha de 1 ATE linha FACA
    para coluna de 1 ATE linha FACA
      escreva(" ");
    PARA coluna FAÇA
      acumulador := 1;
      potencia := 1;
      cont := 1;

      para valor de 1 ATE coluna FACA
        acumulador := acumulador * (linha - valor + 1);
        potencia := potencia * valor;
        cont := cont + 1;
      FIM PARA

      resultado := acumulador DIV potencia;

      escreva(resultado, " ");
    PARA coluna

    escreva("\n");
  PARA linha
FIM PROGRAMA
```

EXPLANATION:

- The program starts by asking the user to enter the number of rows for the Pascal's triangle.


- Then, it iterates through each row and column of the triangle and calculates the corresponding value using the formula:

```
result = (line - value + 1)! / (value!) * (line - value)!
```

- The factorial of a number is the product of all positive integers up to that number. For example, 5! = 5 * 4 * 3 * 2 * 1 = 120.


- The program prints the values in each row, separated by spaces, and then moves to the next row.


- Finally, it prints the completed Pascal's triangle to the console.