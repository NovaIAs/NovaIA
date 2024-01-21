```dart
// Crear una clase llamada Persona con los atributos nombre, edad y género.
class Persona {
  String nombre;
  int edad;
  String género;

  // Constructor de la clase Persona.
  Persona(this.nombre, this.edad, this.género);

  // Método para mostrar la información de la persona.
  void mostrarInformacion() {
    print("Nombre: $nombre");
    print("Edad: $edad");
    print("Género: $género");
  }
}

// Crear una clase llamada Estudiante que hereda de la clase Persona.
class Estudiante extends Persona {
  String carrera;
  int añoDeEstudio;

  // Constructor de la clase Estudiante.
  Estudiante(String nombre, int edad, String género, this.carrera, this.añoDeEstudio) : super(nombre, edad, género);

  // Método para mostrar la información del estudiante.
  void mostrarInformacion() {
    super.mostrarInformacion();
    print("Carrera: $carrera");
    print("Año de estudio: $añoDeEstudio");
  }
}

// Crear una clase llamada Profesor que hereda de la clase Persona.
class Profesor extends Persona {
  String materia;
  String titulación;

  // Constructor de la clase Profesor.
  Profesor(String nombre, int edad, String género, this.materia, this.titulación) : super(nombre, edad, género);

  // Método para mostrar la información del profesor.
  void mostrarInformacion() {
    super.mostrarInformacion();
    print("Materia: $materia");
    print("Titulación: $titulación");
  }
}

// Crear una lista de personas.
List<Persona> personas = [
  Persona("Juan Pérez", 20, "Masculino"),
  Estudiante("María López", 18, "Femenino", "Ingeniería en Informática", 2),
  Profesor("Carlos García", 40, "Masculino", "Matemáticas", "Doctor en Matemáticas")
];

// Recorrer la lista de personas y mostrar la información de cada una.
for (var persona in personas) {
  persona.mostrarInformacion();
  print("");
}
```

Explicación del código:

* Se crean tres clases: `Persona`, `Estudiante` y `Profesor`.
* La clase `Persona` tiene tres atributos: `nombre`, `edad` y `género`.
* La clase `Estudiante` hereda de la clase `Persona` y tiene dos atributos adicionales: `carrera` y `añoDeEstudio`.
* La clase `Profesor` hereda de la clase `Persona` y tiene dos atributos adicionales: `materia` y `titulación`.
* Se crea una lista de personas que contiene objetos de las clases `Persona`, `Estudiante` y `Profesor`.
* Se recorre la lista de personas y se muestra la información de cada una.

Este código es complejo porque utiliza herencia, polimorfismo y genéricos. Es un ejemplo de cómo se pueden crear clases y objetos en Dart.