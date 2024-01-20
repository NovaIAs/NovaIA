```dart
// Definición de la clase "Persona"
class Persona {
  // Atributos privados
  final String _nombre;
  final String _apellidos;
  final int _edad;

  // Constructor
  Persona(String nombre, String apellidos, int edad)
      : _nombre = nombre,
        _apellidos = apellidos,
        _edad = edad;

  // Métodos "get" para acceder a los atributos privados
  String get nombre => _nombre;
  String get apellidos => _apellidos;
  int get edad => _edad;

  // Método para imprimir la información de la persona
  void imprimir() {
    print("Nombre: $_nombre");
    print("Apellidos: $_apellidos");
    print("Edad: $_edad");
  }
}

// Definición de la clase "Estudiante" que hereda de "Persona"
class Estudiante extends Persona {
  // Atributos privados
  final String _carrera;
  final double _promedio;

  // Constructor
  Estudiante(String nombre, String apellidos, int edad, String carrera, double promedio)
      : super(nombre, apellidos, edad),
        _carrera = carrera,
        _promedio = promedio;

  // Métodos "get" para acceder a los atributos privados
  String get carrera => _carrera;
  double get promedio => _promedio;

  // Método para imprimir la información del estudiante
  @override
  void imprimir() {
    super.imprimir();
    print("Carrera: $_carrera");
    print("Promedio: $_promedio");
  }
}

// Definición de la clase "Profesor" que hereda de "Persona"
class Profesor extends Persona {
  // Atributos privados
  final String _departamento;
  final String _cargo;

  // Constructor
  Profesor(String nombre, String apellidos, int edad, String departamento, String cargo)
      : super(nombre, apellidos, edad),
        _departamento = departamento,
        _cargo = cargo;

  // Métodos "get" para acceder a los atributos privados
  String get departamento => _departamento;
  String get cargo => _cargo;

  // Método para imprimir la información del profesor
  @override
  void imprimir() {
    super.imprimir();
    print("Departamento: $_departamento");
    print("Cargo: $_cargo");
  }
}

// Función principal del programa
void main() {
  // Creación de un objeto de la clase "Estudiante"
  var estudiante = Estudiante("Juan", "Pérez", 20, "Ingeniería Informática", 8.5);

  // Creación de un objeto de la clase "Profesor"
  var profesor = Profesor("María", "García", 40, "Departamento de Matemáticas", "Profesora Titular");

  // Impresión de la información del estudiante y del profesor
  estudiante.imprimir();
  print("\n");
  profesor.imprimir();
}
```

Explicación del código:

1. Definimos las clases **Persona**, **Estudiante** y **Profesor** con sus respectivos atributos y métodos.

2. La clase **Persona** es una clase base para las clases **Estudiante** y **Profesor**. Contiene los atributos y métodos comunes a todas las personas, como el nombre, los apellidos y la edad.

3. Las clases **Estudiante** y **Profesor** heredan de la clase **Persona** utilizando la palabra clave `extends`. Esto significa que heredan todos los atributos y métodos de la clase **Persona**. Además, añaden atributos y métodos específicos para cada tipo de persona.

4. En la función `main()` creamos un objeto de la clase **Estudiante** y otro de la clase **Profesor**. Luego, llamamos al método `imprimir()` para mostrar la información de cada objeto.

Este código crea una jerarquía de clases en la que la clase **Persona** es la clase base y las clases **Estudiante** y **Profesor** son clases derivadas. Las clases **Estudiante** y **Profesor** heredan los atributos y métodos de la clase **Persona**, y añaden atributos y métodos específicos para cada tipo de persona.