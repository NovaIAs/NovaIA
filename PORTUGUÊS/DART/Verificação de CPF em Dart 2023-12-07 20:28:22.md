Claro! Aqui está um código em Dart que faz a verificação de um CPF:

```dart
import 'dart:io';

void main() {
  print('Digite o CPF a ser verificado: ');
  String cpf = stdin.readLineSync() ?? '';

  if (validarCpf(cpf)) {
    print('CPF válido.');
  } else {
    print('CPF inválido.');
  }
}

bool validarCpf(String cpf) {
  if (cpf.length != 11 || cpf.contains(RegExp(r'[a-zA-Z]'))) {
    return false;
  }

  List<int> digitos = cpf.split('').map(int.parse).toList();

  if (_verificarDigitosRepetidos(digitos)) {
    return false;
  }

  int soma1 = 0;
  int soma2 = 0;

  for (int i = 0; i < 9; i++) {
    soma1 += digitos[i] * (10 - i);
    soma2 += digitos[i] * (11 - i);
  }

  int resto1 = (soma1 * 10) % 11;
  int resto2 = (soma2 * 10) % 11;

  resto1 = (resto1 == 10) ? 0 : resto1;
  resto2 = (resto2 == 10) ? 0 : resto2;

  if (resto1 != digitos[9] || resto2 != digitos[10]) {
    return false;
  }

  return true;
}

bool _verificarDigitosRepetidos(List<int> digitos) {
  return digitos.every((digit) => digit == digitos[0]);
}
```

Neste código, temos uma função `validarCpf` que recebe uma string `cpf` e verifica se esse CPF é válido. Ele realiza algumas validações:

1. Verifica se o tamanho do CPF é igual a 11 caracteres e se não há letras no cpf usando a função `contains` e expressão regular `RegExp`.
2. Converte cada caractere do cpf em uma lista de inteiros usando `split('')` e `map`.
3. Verifica se todos os dígitos do CPF são iguais usando a função `_verificarDigitosRepetidos`.
4. Calcula a soma dos produtos dos dígitos do CPF pelos pesos correspondentes.
5. Calcula os restos da divisão das somas por 11.
6. Verifica se os restos são iguais aos dígitos verificadores do CPF.

A função `_verificarDigitosRepetidos` verifica se todos os dígitos da lista são iguais, usando a função `every` do Dart.

No método `main`, solicitamos o CPF ao usuário e chamamos a função `validarCpf` para verificar se é válido ou não. Em seguida, mostramos uma mensagem correspondente.

Este código é uma demonstração básica de como verificar a validade de um CPF usando Dart.