```portugol
programa "Calculadora Científica"
{
  função void main()
  {
    variável texto t, v;
    variável real n1, n2, resultado;
    variável inteiro opc;

    escreva("Digite o primeiro número: ");
    leia(n1);

    escreva("Digite o segundo número: ");
    leia(n2);

    escreva("Digite a operação desejada: \n");
    escreva("1. Adição (+)\n");
    escreva("2. Subtração (-)\n");
    escreva("3. Multiplicação (*)\n");
    escreva("4. Divisão (/)\n");
    escreva("5. Potenciação (^)\n");
    escreva("6. Raiz quadrada (√)\n");
    escreva("7. Seno (sin)\n");
    escreva("8. Cosseno (cos)\n");
    escreva("9. Tangente (tan)\n");
    escreva("10. Cotangente (cot)\n");
    escreva("11. Secante (sec)\n");
    escreva("12. Cossecante (csc)\n");
    leia(opc);

    switch (opc)
    {
      case 1:
        resultado = n1 + n2;
        t = "Adição";
        break;
      case 2:
        resultado = n1 - n2;
        t = "Subtração";
        break;
      case 3:
        resultado = n1 * n2;
        t = "Multiplicação";
        break;
      case 4:
        if (n2 == 0)
        {
          escreva("Erro: divisão por zero!\n");
          break;
        }
        resultado = n1 / n2;
        t = "Divisão";
        break;
      case 5:
        resultado = pow(n1, n2);
        t = "Potenciação";
        break;
      case 6:
        if (n1 < 0)
        {
          escreva("Erro: raiz quadrada de número negativo!\n");
          break;
        }
        resultado = sqrt(n1);
        t = "Raiz quadrada";
        break;
      case 7:
        resultado = sin(n1);
        t = "Seno";
        break;
      case 8:
        resultado = cos(n1);
        t = "Cosseno";
        break;
      case 9:
        if (cos(n1) == 0)
        {
          escreva("Erro: tangente de 90 graus!\n");
          break;
        }
        resultado = tan(n1);
        t = "Tangente";
        break;
      case 10:
        if (sin(n1) == 0)
        {
          escreva("Erro: cotangente de 0 graus!\n");
          break;
        }
        resultado = 1 / tan(n1);
        t = "Cotangente";
        break;
      case 11:
        if (cos(n1) == 0)
        {
          escreva("Erro: secante de 90 graus!\n");
          break;
        }
        resultado = 1 / cos(n1);
        t = "Secante";
        break;
      case 12:
        if (sin(n1) == 0)
        {
          escreva("Erro: cossecante de 0 graus!\n");
          break;
        }
        resultado = 1 / sin(n1);
        t = "Cossecante";
        break;
      default:
        escreva("Opção inválida!\n");
        break;
    }

    escreva("\nO resultado da ", t, " é: ", resultado, "\n");
  }
}
```

Este código é uma calculadora científica em PORTUGOL. Ele permite que o utilizador escolha entre várias operações, como adição, subtração, multiplicação, divisão, potenciação, raiz quadrada, seno, cosseno, tangente, cotangente, secante e cossecante.

O código primeiro pede ao utilizador que digite dois números e a operação desejada. Em seguida, ele usa uma instrução `switch` para selecionar a operação correta e calcular o resultado. Finalmente, o código exibe o resultado da operação na tela.

Aqui estão algumas explicações sobre o código:

* A variável `opc` é usada para armazenar a opção escolhida pelo utilizador.
* A variável `resultado` é usada para armazenar o resultado da operação.
* A variável `t` é usada para armazenar o nome da operação.
* A instrução `switch` é usada para selecionar a operação correta e calcular o resultado.
* A função `pow()` é usada para calcular a potência de um número.
* A função `sqrt()` é usada para calcular a raiz quadrada de um número.
* As funções `sin()`, `cos()`, `tan()`, `cot()`, `sec()` e `csc()` são usadas para calcular as funções trigonométricas.

Este código é um exemplo de como usar uma instrução `switch` para selecionar a operação correta e calcular o resultado. Ele também é um exemplo de como usar funções matemáticas para realizar cálculos complexos.