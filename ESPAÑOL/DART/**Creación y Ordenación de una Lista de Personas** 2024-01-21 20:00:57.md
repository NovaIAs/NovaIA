```dart
import 'dart:core';

// Esta clase representa una persona.
class Persona {
  String nombre;
  int edad;

  // Constructor de la clase Persona.
  Persona(this.nombre, this.edad);

  // Método que devuelve el nombre de la persona.
  String getNombre => nombre;

  // Método que devuelve la edad de la persona.
  int getEdad => edad;

  // Método que establece el nombre de la persona.
  setNombre(String nombre) => this.nombre = nombre;

  // Método que establece la edad de la persona.
  setEdad(int edad) => this.edad = edad;

  // Método que devuelve una representación en cadena de la persona.
  @override
  String toString() => 'Persona{nombre: $nombre, edad: $edad}';
}

// Esta clase representa una lista de personas.
class ListaPersonas {
  List<Persona> personas;

  // Constructor de la clase ListaPersonas.
  ListaPersonas(this.personas);

  // Método que devuelve el número de personas en la lista.
  int getLongitud => personas.length;

  // Método que devuelve la persona en la posición especificada.
  Persona getPersona(int posicion) => personas[posicion];

  // Método que añade una persona a la lista.
  void añadirPersona(Persona persona) => personas.add(persona);

  // Método que elimina una persona de la lista.
  void eliminarPersona(Persona persona) => personas.remove(persona);

  // Método que devuelve una representación en cadena de la lista de personas.
  @override
  String toString() => 'ListaPersonas{personas: $personas}';
}

// Esta función crea una lista de personas aleatorias.
ListaPersonas crearListaPersonasAleatoria(int numeroPersonas) {
  List<Persona> personas = [];
  for (int i = 0; i < numeroPersonas; i++) {
    personas.add(Persona('Persona $i', Random().nextInt(100)));
  }
  return ListaPersonas(personas);
}

// Esta función ordena una lista de personas por edad.
void ordenarListaPersonasPorEdad(ListaPersonas listaPersonas) {
  listaPersonas.personas.sort((a, b) => a.edad.compareTo(b.edad));
}

// Esta función imprime una lista de personas en la consola.
void imprimirListaPersonas(ListaPersonas listaPersonas) {
  for (Persona persona in listaPersonas.personas) {
    print(persona);
  }
}

// Esta es la función principal del programa.
void main() {
  // Creamos una lista de 10 personas aleatorias.
  ListaPersonas listaPersonas = crearListaPersonasAleatoria(10);

  // Ordenamos la lista de personas por edad.
  ordenarListaPersonasPorEdad(listaPersonas);

  // Imprimimos la lista de personas en la consola.
  imprimirListaPersonas(listaPersonas);
}
```

**Explicación del código:**

* La clase `Persona` representa a una persona con un nombre y una edad.
* La clase `ListaPersonas` representa a una lista de personas.
* La función `crearListaPersonasAleatoria` crea una lista de personas aleatorias.
* La función `ordenarListaPersonasPorEdad` ordena una lista de personas por edad.
* La función `imprimirListaPersonas` imprime una lista de personas en la consola.
* La función `main` es la función principal del programa. Crea una lista de 10 personas aleatorias, la ordena por edad y la imprime en la consola.

Este código es un ejemplo de cómo utilizar clases y objetos en Dart. También muestra cómo utilizar funciones para realizar tareas específicas.