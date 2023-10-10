Claro! Aqui está um código em Dart que cria uma função para calcular o fatorial de um número:

```dart
import 'dart:io';

void main() {
  stdout.write("Digite um número: ");
  int numero = int.parse(stdin.readLineSync()!);

  int resultado = calcularFatorial(numero);
  print("O fatorial de $numero é $resultado");
}

int calcularFatorial(int n) {
  if (n == 0 || n == 1) {
    return 1;
  } else {
    int fatorial = 1;
    for (int i = 2; i <= n; i++) {
      fatorial *= i;
    }
    return fatorial;
  }
}
```

Neste código, começamos importando a biblioteca `dart:io` para usar a função `stdin` e `stdout`. A função `stdin` permite ler a entrada do usuário, enquanto a função `stdout` permite enviar saídas para o terminal.

Em seguida, temos a função `main`, que é o ponto de entrada do programa. Dentro dessa função, pedimos ao usuário para digitar um número e armazenamos o valor em uma variável chamada `numero`.

Em seguida, chamamos a função `calcularFatorial` passando o número digitado como argumento. O valor retornado por essa função é armazenado na variável `resultado`.

Por fim, exibimos a mensagem informando o fatorial do número digitado.

A função `calcularFatorial` recebe um número `n` como argumento. Primeiro, verificamos se `n` é igual a 0 ou 1. Se sim, retornamos 1, pois o fatorial de 0 e 1 é sempre 1.

Caso contrário, inicializamos uma variável `fatorial` com o valor 1. Em seguida, usamos um loop for para multiplicar `fatorial` pelos números de 2 até `n`. O resultado final é o fatorial do número `n`, que é retornado pela função.