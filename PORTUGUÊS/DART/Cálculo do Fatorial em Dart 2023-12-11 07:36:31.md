Claro! Aqui está um exemplo de um código complexo em Dart que calcula o fatorial de um número:

```dart
import 'dart:io';

void main() {
  stdout.write('Digite um número inteiro positivo: ');
  int numero = int.parse(stdin.readLineSync()!);

  int fatorial = calcularFatorial(numero);
  print('O fatorial de $numero é: $fatorial');
}

int calcularFatorial(int numero) {
  if (numero == 0 || numero == 1) {
    return 1;
  } else {
    return numero * calcularFatorial(numero - 1);
  }
}
```

Neste código, começamos importando a biblioteca `dart:io`, que permite a interação com o usuário através do console.

Em seguida, temos a função `main` que será chamada quando o programa for executado. Dentro da função `main`, usamos `stdout.write` para exibir uma mensagem solicitando ao usuário que digite um número inteiro positivo. Em seguida, usamos `stdin.readLineSync()` para ler a entrada do usuário e `int.parse` para converter a entrada para um número inteiro.

Depois disso, chamamos a função `calcularFatorial` passando o número digitado pelo usuário como argumento. O resultado do cálculo é armazenado na variável `fatorial` e, por fim, utilizamos `print` para exibir o resultado na tela.

A função `calcularFatorial` é uma função recursiva que recebe um número como parâmetro. Se o número for 0 ou 1, o fatorial é 1, então a função retorna 1. Caso contrário, a função chama-se novamente, passando o número decrementado em 1, e multiplica o número atual pelo resultado da chamada recursiva.

Espero que este código atenda às suas expectativas!