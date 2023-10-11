Claro! Aqui está um código complexo em Dart que utiliza conceitos avançados como classes, herança e polimorfismo:

```dart
import 'dart:math';

void main() {
  // Cria uma instância da classe Figura
  Figura figura = new Figura();

  // Gera uma forma aleatória
  Random random = new Random();
  int formaAleatoria = random.nextInt(3);

  // Verifica qual forma foi gerada
  if (formaAleatoria == 0) {
    figura = new Circulo(raio: 5.0);
  } else if (formaAleatoria == 1) {
    figura = new Retangulo(largura: 10.0, altura: 5.0);
  } else {
    figura = new Triangulo(base: 8.0, altura: 6.0);
  }

  // Chama o método para calcular a área da forma
  double area = figura.calcularArea();

  // Imprime o resultado
  print("A área da forma gerada é $area");
}

// Classe base Figura
class Figura {
  double calcularArea() {
    return 0.0;
  }
}

// Classe Circulo que herda da classe Figura
class Circulo extends Figura {
  double raio;

  Circulo({required this.raio});

  @override
  double calcularArea() {
    return pi * raio * raio;
  }
}

// Classe Retangulo que herda da classe Figura
class Retangulo extends Figura {
  double largura;
  double altura;

  Retangulo({required this.largura, required this.altura});

  @override
  double calcularArea() {
    return largura * altura;
  }
}

// Classe Triangulo que herda da classe Figura
class Triangulo extends Figura {
  double base;
  double altura;

  Triangulo({required this.base, required this.altura});

  @override
  double calcularArea() {
    return (base * altura) / 2;
  }
}
```

Neste código, é criada uma classe base chamada `Figura`, que possui um método `calcularArea()` que retorna 0.0 por padrão. Em seguida, são criadas três classes que herdam da classe `Figura`: `Circulo`, `Retangulo` e `Triangulo`. Essas classes implementam o método `calcularArea()` de acordo com a fórmula de cada forma geométrica.

No método `main()`, é criada uma instância da classe `Figura` e é gerada uma forma aleatória (círculo, retângulo ou triângulo) usando a classe `Random` do pacote `dart:math`. Em seguida, é chamado o método `calcularArea()` da forma gerada e o resultado é impresso na tela.

Esse código demonstra a utilização de classes, herança e polimorfismo para calcular a área de diferentes formas geométricas de maneira flexível e eficiente.