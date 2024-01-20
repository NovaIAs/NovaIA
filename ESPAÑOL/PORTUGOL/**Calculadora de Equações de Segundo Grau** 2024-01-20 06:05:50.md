```portuguol
programa "Calculadora de Equações de Segundo Grau"

def double calculaDiscriminant(double a, double b, double c) {
  return b * b - 4 * a * c;
}

def double[] calculaRaizes(double a, double b, double c) {
  double discriminant = calculaDiscriminant(a, b, c);
  double[] roots = new double[2];

  if (discriminant > 0) {
    roots[0] = (-b + sqrt(discriminant)) / (2 * a);
    roots[1] = (-b - sqrt(discriminant)) / (2 * a);
  } else if (discriminant == 0) {
    roots[0] = -b / (2 * a);
    roots[1] = roots[0];
  }

  return roots;
}

def void main() {
  while (true) {
    print("Digite os coeficientes da equação de segundo grau (a, b, c): ");
    double a = double.parse(readLine());
    double b = double.parse(readLine());
    double c = double.parse(readLine());

    double[] roots = calculaRaizes(a, b, c);

    if (roots.length == 2) {
      print("As raízes da equação são: ");
      print("x1 = " + roots[0]);
      print("x2 = " + roots[1]);
    } else if (roots.length == 1) {
      print("A equação possui apenas uma raiz real: ");
      print("x = " + roots[0]);
    } else {
      print("A equação não possui raízes reais.");
    }

    print("Deseja continuar? (s/n): ");
    String answer = readLine();
    if (answer.equals("n")) {
      break;
    }
  }
}
```

Este código é uma calculadora de equações de segundo grau em PORTUGOL. Ele solicita ao usuário os coeficientes da equação (a, b e c) e calcula as raízes da equação usando a fórmula de Bhaskara.

O código é dividido em três funções:

* `calculaDiscriminant`: calcula o discriminante da equação, que é um valor que determina se a equação possui raízes reais ou não.
* `calculaRaizes`: calcula as raízes da equação usando a fórmula de Bhaskara.
* `main`: a função principal do programa, que solicita ao usuário os coeficientes da equação, chama as funções `calculaDiscriminant` e `calculaRaizes`, e imprime as raízes da equação.

O código também usa algumas classes e funções do PORTUGOL, como `double`, `readLine`, `print` e `sqrt`.