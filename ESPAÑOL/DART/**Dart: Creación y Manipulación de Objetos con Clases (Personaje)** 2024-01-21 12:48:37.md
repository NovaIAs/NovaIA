```dart
// Primero, creamos una clase llamada 'Persona'
class Persona {
  String nombre;
  int edad;

  // Constructor de la clase
  Persona(this.nombre, this.edad);

  // Método para obtener el nombre de la persona
  String getNombre() {
    return this.nombre;
  }

  // Método para obtener la edad de la persona
  int getEdad() {
    return this.edad;
  }

  // Método para cambiar el nombre de la persona
  void setNombre(String nuevoNombre) {
    this.nombre = nuevoNombre;
  }

  // Método para cambiar la edad de la persona
  void setEdad(int nuevaEdad) {
    this.edad = nuevaEdad;
  }
}

// Luego, creamos una clase llamada 'Impresora'
class Impresora {
  // Método para imprimir una persona
  void imprimirPersona(Persona persona) {
    print('Nombre: ${persona.getNombre()}');
    print('Edad: ${persona.getEdad()}');
  }
}

// Ahora, creamos una función llamada 'crearPersona'
Persona crearPersona(String nombre, int edad) {
  return Persona(nombre, edad);
}

// Finalmente, creamos una función llamada 'main'
void main() {
  // Creamos una instancia de la clase 'Persona'
  var persona1 = crearPersona('Juan', 25);

  // Creamos una instancia de la clase 'Impresora'
  var impresora1 = Impresora();

  // Imprimimos la persona usando la impresora
  impresora1.imprimirPersona(persona1);