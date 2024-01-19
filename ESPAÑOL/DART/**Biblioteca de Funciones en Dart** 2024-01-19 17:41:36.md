```dart
// Importamos las librerías necesarias.
import 'dart:io';
import 'dart:math';

// Creamos una clase para manejar los archivos.
class Archivo {
  // Creamos un constructor que reciba el nombre del archivo.
  Archivo(String nombre) {
    // Abrimos el archivo en modo lectura.
    this.file = new File(nombre).openSync(mode: FileMode.READ);
  }

  // Creamos un método para leer el archivo.
  String leer() {
    // Leemos el archivo y lo convertimos a una cadena de caracteres.
    String contenido = this.file.readAsStringSync();

    // Cerramos el archivo.
    this.file.closeSync();

    // Devolvemos el contenido del archivo.
    return contenido;
  }

  // Creamos un método para escribir en el archivo.
  void escribir(String contenido) {
    // Abrimos el archivo en modo escritura.
    this.file = new File(nombre).openSync(mode: FileMode.WRITE);

    // Escribimos el contenido en el archivo.
    this.file.writeAsStringSync(contenido);

    // Cerramos el archivo.
    this.file.closeSync();
  }

  // Creamos una variable privada para almacenar el archivo.
  File file;
}

// Creamos una clase para manejar los números.
class Numero {
  // Creamos un constructor que reciba el valor del número.
  Numero(num valor) {
    // Almacenamos el valor del número en una variable privada.
    this.valor = valor;
  }

  // Creamos un método para sumar dos números.
  Numero suma(Numero otro) {
    // Devolvemos un nuevo objeto Numero con la suma de los dos números.
    return new Numero(this.valor + otro.valor);
  }

  // Creamos un método para restar dos números.
  Numero resta(Numero otro) {
    // Devolvemos un nuevo objeto Numero con la resta de los dos números.
    return new Numero(this.valor - otro.valor);
  }

  // Creamos un método para multiplicar dos números.
  Numero multiplica(Numero otro) {
    // Devolvemos un nuevo objeto Numero con la multiplicación de los dos números.
    return new Numero(this.valor * otro.valor);
  }

  // Creamos un método para dividir dos números.
  Numero divide(Numero otro) {
    // Devolvemos un nuevo objeto Numero con la división de los dos números.
    return new Numero(this.valor / otro.valor);
  }

  // Creamos una variable privada para almacenar el valor del número.
  num valor;
}

// Creamos una función para generar un número aleatorio.
num aleatorio() {
  // Creamos un objeto Random para generar números aleatorios.
  Random random = new Random();

  // Devolvemos un número aleatorio entre 0 y 1.
  return random.nextDouble();
}

// Creamos una función para calcular el factorial de un número.
num factorial(num n) {
  // Si n es 0 o 1, devolvemos 1.
  if (n == 0 || n == 1) {
    return 1;
  }

  // Si n es mayor que 1, calculamos el factorial de n llamando a la función recursivamente.
  else {
    return n * factorial(n - 1);
  }
}

// Creamos una función para calcular el máximo común divisor de dos números.
num mcd(num a, num b) {
  // Si b es 0, devolvemos a.
  if (b == 0) {
    return a;
  }

  // Si b no es 0, calculamos el máximo común divisor de b y el resto de la división de a entre b.
  else {
    return mcd(b, a % b);
  }
}

// Creamos una función para calcular el mínimo común múltiplo de dos números.
num mcm(num a, num b) {
  // Calculamos el máximo común divisor de a y b.
  num mcd = mcd(a, b);

  // Calculamos el mínimo común múltiplo de a y b dividiendo el producto de a y b por el máximo común divisor.
  num mcm = (a * b) / mcd;

  // Devolvemos el mínimo común múltiplo.
  return mcm;
}

// Creamos una función para leer un número del teclado.
num leerNumero() {
  // Leemos una línea de texto del teclado.
  String linea = stdin.readLineSync();

  // Convertimos la línea de texto a un número.
  num numero = num.parse(linea);

  // Devolvemos el número.
  return numero;
}

// Creamos una función para imprimir un número en la consola.
void imprimirNumero(num numero) {
  // Imprimimos el número en la consola.
  print(numero);
}

// Creamos una función para calcular el área de un triángulo.
num areaTriangulo(num base, num altura) {
  // Calculamos el área del triángulo multiplicando la base por la altura y dividiendo el resultado entre 2.
  num area = (base * altura) / 2;

  // Devolvemos el área del triángulo.
  return area;
}

// Creamos una función para calcular el perímetro de un triángulo.
num perimetroTriangulo(num lado1, num lado2, num lado3) {
  // Calculamos el perímetro del triángulo sumando los tres lados.
  num perimetro = lado1 + lado2 + lado