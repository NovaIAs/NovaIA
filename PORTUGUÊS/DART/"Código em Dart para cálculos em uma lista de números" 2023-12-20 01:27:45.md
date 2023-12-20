Claro! Aqui está um exemplo de código complexo em Dart:

```dart
import 'dart:math';

void main() {
  var listaNumeros = [1, 2, 3, 4, 5];
  
  var soma = calcularSoma(listaNumeros);
  print('A soma dos números é: $soma');
  
  var media = calcularMedia(listaNumeros);
  print('A média dos números é: $media');
  
  var maximo = encontrarMaximo(listaNumeros);
  print('O maior número da lista é: $maximo');
  
  var minimo = encontrarMinimo(listaNumeros);
  print('O menor número da lista é: $minimo');
  
  var numerosPares = encontrarNumerosPares(listaNumeros);
  print('Os números pares da lista são: $numerosPares');
  
  var numerosImpares = encontrarNumerosImpares(listaNumeros);
  print('Os números ímpares da lista são: $numerosImpares');
  
  var numerosPrimos = encontrarNumerosPrimos(listaNumeros);
  print('Os números primos da lista são: $numerosPrimos');
}

int calcularSoma(List<int> numeros) {
  var soma = 0;
  for (var numero in numeros) {
    soma += numero;
  }
  return soma;
}

double calcularMedia(List<int> numeros) {
  var soma = calcularSoma(numeros);
  var media = soma / numeros.length;
  return media;
}

int encontrarMaximo(List<int> numeros) {
  var maximo = numeros[0];
  for (var i = 1; i < numeros.length; i++) {
    if (numeros[i] > maximo) {
      maximo = numeros[i];
    }
  }
  return maximo;
}

int encontrarMinimo(List<int> numeros) {
  var minimo = numeros[0];
  for (var i = 1; i < numeros.length; i++) {
    if (numeros[i] < minimo) {
      minimo = numeros[i];
    }
  }
  return minimo;
}

List<int> encontrarNumerosPares(List<int> numeros) {
  var numerosPares = [];
  for (var numero in numeros) {
    if (numero % 2 == 0) {
      numerosPares.add(numero);
    }
  }
  return numerosPares;
}

List<int> encontrarNumerosImpares(List<int> numeros) {
  var numerosImpares = [];
  for (var numero in numeros) {
    if (numero % 2 != 0) {
      numerosImpares.add(numero);
    }
  }
  return numerosImpares;
}

List<int> encontrarNumerosPrimos(List<int> numeros) {
  var numerosPrimos = [];
  for (var numero in numeros) {
    if (ePrimo(numero)) {
      numerosPrimos.add(numero);
    }
  }
  return numerosPrimos;
}

bool ePrimo(int numero) {
  if (numero <= 1) {
    return false;
  }
  for (var i = 2; i <= sqrt(numero); i++) {
    if (numero % i == 0) {
      return false;
    }
  }
  return true;
}
```

Neste código, temos uma lista de números e uma série de funções para realizar operações sobre essa lista. A função `calcularSoma` recebe a lista de números e retorna a soma de todos eles. A função `calcularMedia` utiliza a função `calcularSoma` para calcular a média dos números. As funções `encontrarMaximo` e `encontrarMinimo` retornam o maior e o menor número da lista, respectivamente.

As funções `encontrarNumerosPares` e `encontrarNumerosImpares` percorrem a lista de números e retornam uma nova lista contendo apenas os números pares e ímpares, respectivamente. A função `encontrarNumerosPrimos` utiliza a função `ePrimo` para verificar se um número é primo e retorna uma nova lista contendo apenas os números primos da lista original.

A função `ePrimo` verifica se um número é primo, percorrendo todos os números de 2 até a raiz quadrada do número e verificando se algum deles é divisor do número em questão.

Espero que este exemplo atenda às suas expectativas!