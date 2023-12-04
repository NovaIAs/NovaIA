Claro! Aqui está um exemplo de código complexo em Dart:

```dart
import 'dart:math';

void main() {
  List<int> numeros = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20];
  
  List<int> numerosPrimos = [];
  
  for (int num in numeros) {
    if (isPrimo(num)) {
      numerosPrimos.add(num);
    }
  }
  
  print("Números primos encontrados: $numerosPrimos");
  
  int somaPrimos = calcularSoma(numerosPrimos);
  
  print("A soma dos números primos é: $somaPrimos");
  
  List<int> numerosPares = [];
  
  for (int num in numeros) {
    if (isPar(num)) {
      numerosPares.add(num);
    }
  }
  
  print("Números pares encontrados: $numerosPares");
  
  int produtoPares = calcularProduto(numerosPares);
  
  print("O produto dos números pares é: $produtoPares");
  
  int maiorNumero = calcularMaiorNumero(numeros);
  
  print("O maior número é: $maiorNumero");
}

bool isPrimo(int numero) {
  if (numero <= 1) {
    return false;
  }
  
  for (int i = 2; i <= sqrt(numero); i++) {
    if (numero % i == 0) {
      return false;
    }
  }
  
  return true;
}

bool isPar(int numero) {
  return numero % 2 == 0;
}

int calcularSoma(List<int> numeros) {
  int soma = 0;
  
  for (int num in numeros) {
    soma += num;
  }
  
  return soma;
}

int calcularProduto(List<int> numeros) {
  int produto = 1;
  
  for (int num in numeros) {
    produto *= num;
  }
  
  return produto;
}

int calcularMaiorNumero(List<int> numeros) {
  int maiorNumero = numeros[0];
  
  for (int i = 1; i < numeros.length; i++) {
    if (numeros[i] > maiorNumero) {
      maiorNumero = numeros[i];
    }
  }
  
  return maiorNumero;
}
```

Neste código, temos uma lista de números e realizamos várias operações com eles. Primeiro, filtramos os números primos da lista e calculamos a soma desses números. Em seguida, filtramos os números pares da lista e calculamos o produto desses números. Também encontramos o maior número da lista. Todos os resultados são exibidos no console.