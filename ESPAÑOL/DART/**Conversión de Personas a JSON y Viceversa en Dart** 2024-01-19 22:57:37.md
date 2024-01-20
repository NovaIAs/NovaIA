```dart
// Importamos la biblioteca de 'dart:convert' para trabajar con JSON.
import 'dart:convert';

// Creamos una clase llamada 'Persona' que representa a una persona con sus atributos.
class Persona {
  String nombre;
  int edad;
  String ciudad;

  // Creamos un constructor para la clase 'Persona' que recibe los valores de los atributos.
  Persona(this.nombre, this.edad, this.ciudad);

  // Creamos un método para obtener el nombre completo de la persona.
  String getNombreCompleto() {
    return "$nombre $edad";
  }

  // Creamos un método para obtener la edad de la persona.
  int getEdad() {
    return edad;
  }

  // Creamos un método para obtener la ciudad de la persona.
  String getCiudad() {
    return ciudad;
  }

  // Creamos un método para convertir la persona a un objeto JSON.
  Map<String, dynamic> toJson() {
    return {
      'nombre': nombre,
      'edad': edad,
      'ciudad': ciudad,
    };
  }

  // Creamos un método para crear una persona a partir de un objeto JSON.
  factory Persona.fromJson(Map<String, dynamic> json) {
    return Persona(json['nombre'], json['edad'], json['ciudad']);
  }
}

// Creamos una función que recibe una lista de personas y las convierte a una lista de objetos JSON.
List<Map<String, dynamic>> personasToJson(List<Persona> personas) {
  return personas.map((persona) => persona.toJson()).toList();
}

// Creamos una función que recibe una lista de objetos JSON y los convierte a una lista de personas.
List<Persona> personasFromJson(List<Map<String, dynamic>> json) {
  return json.map((personaJson) => Persona.fromJson(personaJson)).toList();
}

// Creamos una función principal que se ejecuta al iniciar el programa.
void main() {
  // Creamos una lista de personas.
  List<Persona> personas = [
    Persona('Juan', 20, 'Madrid'),
    Persona('María', 25, 'Barcelona'),
    Persona('Pedro', 30, 'Valencia'),
  ];

  // Convertimos la lista de personas a una lista de objetos JSON.
  List<Map<String, dynamic>> personasJson = personasToJson(personas);

  // Mostramos la lista de objetos JSON.
  print(personasJson);

  // Convertimos la lista de objetos JSON a una lista de personas.
  List<Persona> personas2 = personasFromJson(personasJson);

  // Mostramos la lista de personas.
  print(personas2);
}
```

Este código crea una clase llamada 'Persona' que representa a una persona con sus atributos. Luego, crea una función que recibe una lista de personas y las convierte a una lista de objetos JSON, y otra función que recibe una lista de objetos JSON y los convierte a una lista de personas. Por último, crea una función principal que se ejecuta al iniciar el programa y que crea una lista de personas, la convierte a una lista de objetos JSON, la muestra por consola, la convierte de nuevo a una lista de personas y la muestra de nuevo por consola.