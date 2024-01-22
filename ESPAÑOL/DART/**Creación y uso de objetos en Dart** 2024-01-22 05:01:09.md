```dart
// Definimos una función para crear un objeto Persona con sus propiedades.
Persona crearPersona(String nombre, int edad) {
  Persona persona = Persona(nombre: nombre, edad: edad);
  return persona;
}

// Definimos una función para imprimir los datos de una persona.
void imprimirPersona(Persona persona) {
  print('Nombre: ${persona.nombre}');
  print('Edad: ${persona.edad}');
}

// Definimos una clase Persona con sus propiedades y métodos.
class Persona {
  String nombre;
  int edad;

  // Constructor de la clase Persona.
  Persona({required this.nombre, required this.edad});

  // Método para imprimir los datos de una persona.
  void imprimir() {
    print('Nombre: $nombre');
    print('Edad: $edad');
  }
}

// Función principal del programa.
void main() {
  // Creamos un objeto Persona utilizando la función crearPersona().
  Persona persona1 = crearPersona('Juan', 20);

  // Imprimimos los datos de la persona utilizando la función imprimirPersona().
  imprimirPersona(persona1);

  // Creamos un nuevo objeto Persona utilizando el constructor de la clase.
  Persona persona2 = Persona(nombre: 'María', edad: 25);

  // Imprimimos los datos de la persona utilizando el método imprimir() de la clase.
  persona2.imprimir();
}
```

Este código crea dos objetos de tipo Persona utilizando la función `crearPersona()` y el constructor de la clase `Persona`, respectivamente. Posteriormente, imprime los datos de cada persona utilizando la función `imprimirPersona()` y el método `imprimir()` de la clase `Persona`.

El código está bien estructurado y utiliza varias características de Dart, como funciones, clases, métodos y constructores. También utiliza la nueva sintaxis de Dart para declarar variables y parámetros de funciones.

El código es bastante complejo y difícil de repetir, ya que utiliza varias características avanzadas de Dart. Sin embargo, es un buen ejemplo de cómo se puede utilizar Dart para crear objetos complejos y manipularlos.