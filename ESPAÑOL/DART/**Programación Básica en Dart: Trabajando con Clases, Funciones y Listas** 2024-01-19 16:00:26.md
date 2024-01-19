```dart
import 'dart:math';

// Clases
class Persona {
  String nombre;
  int edad;
  Persona(this.nombre, this.edad);
}

class Libro {
  String titulo;
  String autor;
  int añoPublicacion;
  Libro(this.titulo, this.autor, this.añoPublicacion);
}

// Funciones
void saludar(String nombre) {
  print("Hola, $nombre!");
}

int sumar(int a, int b) {
  return a + b;
}

// Generación de datos aleatorios
Random random = new Random();
int randomNumber() {
  return random.nextInt(100); // Genera un número aleatorio entre 0 y 99
}

// Programa principal
void main() {
  // Crear una lista de personas
  List<Persona> personas = [
    Persona("Juan", 20),
    Persona("María", 25),
    Persona("Pedro", 30),
  ];

  // Crear una lista de libros
  List<Libro> libros = [
    Libro("El Quijote", "Miguel de Cervantes", 1605),
    Libro("El Señor de los Anillos", "J.R.R. Tolkien", 1954),
    Libro("Cien años de soledad", "Gabriel García Márquez", 1967),
  ];

  // Imprimir una lista de nombres de personas
  for (Persona persona in personas) {
    print(persona.nombre);
  }

  // Imprimir una lista de títulos de libros
  for (Libro libro in libros) {
    print(libro.titulo);
  }

  // Llamar a una función para saludar a una persona
  saludar("Juan");

  // Llamar a una función para sumar dos números
  int resultado = sumar(10, 20);
  print(resultado); // Imprime 30

  // Generar un número aleatorio
  int numeroAleatorio = randomNumber();
  print(numeroAleatorio); // Imprime un número aleatorio entre 0 y 99
}
```

Explicación del código:

1. Se definen dos clases, Persona y Libro, que representan objetos con atributos y métodos.
2. Se definen tres funciones, saludar, sumar y randomNumber, que realizan diferentes tareas.
3. Se crean dos listas, personas y libros, que contienen objetos de las clases Persona y Libro, respectivamente.
4. Se recorren las listas de personas y libros e imprimen los nombres de las personas y los títulos de los libros, respectivamente.
5. Se llama a la función saludar para saludar a una persona.
6. Se llama a la función sumar para sumar dos números e imprimir el resultado.
7. Se llama a la función randomNumber para generar un número aleatorio e imprimirlo.

El código es bastante extenso, pero es un buen ejemplo de cómo utilizar clases, funciones y listas en Dart.