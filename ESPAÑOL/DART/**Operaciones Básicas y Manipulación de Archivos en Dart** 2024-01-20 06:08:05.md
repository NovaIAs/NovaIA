```
// Importamos las bibliotecas necesarias
import 'dart:io';
import 'dart:math';
import 'dart:async';

// Definimos una función para generar un número aleatorio entre dos valores
double generarNumeroAleatorio(double min, double max) {
  return min + Random().nextDouble() * (max - min);
}

// Definimos una función para calcular el área de un triángulo
double calcularAreaTriangulo(double base, double altura) {
  return 0.5 * base * altura;
}

// Definimos una clase para representar un punto en el espacio
class Punto {
  double x;
  double y;

  // Constructor de la clase
  Punto(this.x, this.y);

  // Método para calcular la distancia entre dos puntos
  double distancia(Punto otroPunto) {
    return sqrt(pow(x - otroPunto.x, 2) + pow(y - otroPunto.y, 2));
  }
}

// Definimos una función para calcular el área de un círculo
double calcularAreaCirculo(double radio) {
  return pi * pow(radio, 2);
}

// Definimos una función para calcular el volumen de una esfera
double calcularVolumenEsfera(double radio) {
  return (4 / 3) * pi * pow(radio, 3);
}

// Función principal de la aplicación
void main() async {
  // Generamos un número aleatorio entre 1 y 10
  double numeroAleatorio = generarNumeroAleatorio(1, 10);

  // Calculamos el área de un triángulo con base 5 y altura 3
  double areaTriangulo = calcularAreaTriangulo(5, 3);

  // Creamos dos puntos en el espacio
  Punto punto1 = Punto(1, 2);
  Punto punto2 = Punto(4, 6);

  // Calculamos la distancia entre los dos puntos
  double distanciaPuntos = punto1.distancia(punto2);

  // Calculamos el área de un círculo con radio 5
  double areaCirculo = calcularAreaCirculo(5);

  // Calculamos el volumen de una esfera con radio 3
  double volumenEsfera = calcularVolumenEsfera(3);

  // Imprimimos los resultados en la consola
  print('Número aleatorio: $numeroAleatorio');
  print('Área del triángulo: $areaTriangulo');
  print('Distancia entre los puntos: $distanciaPuntos');
  print('Área del círculo: $areaCirculo');
  print('Volumen de la esfera: $volumenEsfera');


  // Creamos un archivo en el disco duro
  File file = File('archivo.txt');

  // Escribimos una línea en el archivo
  await file.writeAsString('Hola, mundo!');

  // Leemos el contenido del archivo
  String contenidoArchivo = await file.readAsString();

  // Imprimimos el contenido del archivo en la consola
  print('Contenido del archivo: $contenidoArchivo');

  // Creamos un temporizador
  Timer timer = Timer(Duration(seconds: 5), () {
    print('¡Han pasado 5 segundos!');
  });
}
```

Explicación del código:

- El código comienza importando las bibliotecas necesarias para el funcionamiento del programa.

- Se define una función para generar un número aleatorio entre dos valores.

- Se define una función para calcular el área de un triángulo.

- Se define una clase para representar un punto en el espacio.

- Se define una función para calcular la distancia entre dos puntos.

- Se define una función para calcular el área de un círculo.

- Se define una función para calcular el volumen de una esfera.

- En la función `main()` se genera un número aleatorio, se calcula el área de un triángulo, la distancia entre dos puntos, el área de un círculo y el volumen de una esfera.

- Se crea un archivo en el disco duro y se escribe una línea en él.

- Se lee el contenido del archivo y se imprime en la consola.

- Se crea un temporizador que se activa después de 5 segundos e imprime un mensaje en la consola.