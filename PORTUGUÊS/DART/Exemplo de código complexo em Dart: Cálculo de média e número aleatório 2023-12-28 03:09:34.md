Claro! Aqui está um exemplo de um código complexo em Dart, explicado linha por linha:

```dart
import 'dart:math';

void main() {
  // Inicialização das variáveis
  int numero1 = 10;
  int numero2 = 20;
  double resultado = 0.0;

  // Chamada da função para realizar o cálculo
  resultado = calcularMedia(numero1, numero2);

  // Exibição do resultado
  print("A média dos números $numero1 e $numero2 é: $resultado");

  // Chamada da função para gerar um número aleatório
  int numeroAleatorio = gerarNumeroAleatorio(1, 100);

  // Exibição do número aleatório gerado
  print("Número aleatório: $numeroAleatorio");
}

// Função para calcular a média de dois números
double calcularMedia(int a, int b) {
  return (a + b) / 2;
}

// Função para gerar um número aleatório dentro de um intervalo
int gerarNumeroAleatorio(int min, int max) {
  Random random = Random();
  return min + random.nextInt(max - min);
}
```

Explicação do código:

1. A primeira linha `import 'dart:math';` importa a biblioteca `dart:math`, que é necessária para usar a classe `Random` para gerar números aleatórios.

2. A função `main()` é o ponto de entrada do programa. Ela é executada quando o programa é iniciado.

3. A partir da linha `int numero1 = 10;`, são inicializadas duas variáveis `numero1` e `numero2` com os valores 10 e 20, respectivamente. Também é criada a variável `resultado` com valor inicial 0.0.

4. A linha `resultado = calcularMedia(numero1, numero2);` chama a função `calcularMedia()` passando os valores de `numero1` e `numero2` como argumentos e atribui o resultado à variável `resultado`.

5. A linha `print("A média dos números $numero1 e $numero2 é: $resultado");` exibe uma mensagem na tela com o valor da média dos números.

6. A linha `int numeroAleatorio = gerarNumeroAleatorio(1, 100);` chama a função `gerarNumeroAleatorio()` passando os valores 1 e 100 como argumentos e atribui o número aleatório gerado à variável `numeroAleatorio`.

7. A linha `print("Número aleatório: $numeroAleatorio");` exibe o número aleatório gerado na tela.

8. A função `calcularMedia(int a, int b)` recebe dois números inteiros `a` e `b` como parâmetros e retorna a média desses números.

9. A função `gerarNumeroAleatorio(int min, int max)` recebe dois números inteiros `min` e `max` como parâmetros e retorna um número aleatório dentro do intervalo definido por esses valores.

Espero que esse código complexo em Dart atenda às suas expectativas! Se você tiver alguma outra solicitação, é só me dizer.