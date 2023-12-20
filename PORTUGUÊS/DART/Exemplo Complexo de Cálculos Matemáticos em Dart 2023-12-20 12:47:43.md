Claro! Vou gerar um exemplo complexo em Dart para você. Neste exemplo, vamos criar uma classe chamada "Calculadora" que terá métodos para realizar cálculos matemáticos básicos. Vamos implementar métodos para somar, subtrair, multiplicar e dividir números inteiros. Além disso, também teremos um método para calcular o fatorial de um número. Vou explicar o código linha por linha para você:

```dart
class Calculadora {
  int somar(int a, int b) {
    return a + b;
  }

  int subtrair(int a, int b) {
    return a - b;
  }

  int multiplicar(int a, int b) {
    return a * b;
  }

  double dividir(int a, int b) {
    if (b == 0) {
      throw Exception("Não é possível dividir por zero.");
    }
    return a / b;
  }

  int fatorial(int n) {
    if (n < 0) {
      throw Exception("Não é possível calcular o fatorial de um número negativo.");
    }
    if (n == 0) {
      return 1;
    }
    return n * fatorial(n - 1);
  }
}

void main() {
  Calculadora calculadora = Calculadora();

  int resultadoSoma = calculadora.somar(5, 3);
  print("Resultado da soma: $resultadoSoma");

  int resultadoSubtracao = calculadora.subtrair(10, 7);
  print("Resultado da subtração: $resultadoSubtracao");

  int resultadoMultiplicacao = calculadora.multiplicar(4, 6);
  print("Resultado da multiplicação: $resultadoMultiplicacao");

  double resultadoDivisao = calculadora.dividir(15, 3);
  print("Resultado da divisão: $resultadoDivisao");

  int resultadoFatorial = calculadora.fatorial(5);
  print("Resultado do fatorial: $resultadoFatorial");
}
```

Explicação do código:

- Na linha 1, declaramos a classe "Calculadora".
- Na linha 2, declaramos o método "somar" que recebe dois parâmetros inteiros, "a" e "b", e retorna a soma desses valores.
- Nas linhas 6, 11 e 16, declaramos os métodos "subtrair", "multiplicar" e "dividir", respectivamente, seguindo a mesma lógica do método "somar".
- Na linha 21, declaramos o método "fatorial" que recebe um parâmetro inteiro, "n", e retorna o fatorial desse número.
- Dentro do método "dividir", na linha 9, verificamos se o divisor é zero. Caso seja, lançamos uma exceção informando que não é possível dividir por zero.
- Dentro do método "fatorial", nas linhas 14 e 16, verificamos se o número é negativo ou se é igual a zero, respectivamente. Caso seja, lançamos uma exceção informando que não é possível calcular o fatorial nessas condições.
- No método "main", inicializamos uma instância da classe "Calculadora" na linha 26.
- Nas linhas 29 a 34, utilizamos os métodos da calculadora para realizar diferentes cálculos, armazenando os resultados em variáveis e imprimindo-os na tela.

Espero que este exemplo complexo em Dart tenha sido útil para você!