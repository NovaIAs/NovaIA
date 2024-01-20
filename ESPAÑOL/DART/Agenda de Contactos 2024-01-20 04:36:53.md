```dart
// Importamos la librería 'dart:io' para manejar las entradas y salidas estándar.
import 'dart:io';

// Definimos una clase llamada 'Agenda' que nos permitirá almacenar y gestionar los contactos.
class Agenda {
  // Creamos una lista llamada '_contactos' para almacenar los contactos.
  List<Contacto> _contactos = [];

  // Añadimos un método llamado 'agregarContacto' que nos permitirá añadir un nuevo contacto a la agenda.
  void agregarContacto(Contacto contacto) {
    // Añadimos el contacto a la lista de contactos.
    _contactos.add(contacto);
  }

  // Añadimos un método llamado 'eliminarContacto' que nos permitirá eliminar un contacto de la agenda.
  void eliminarContacto(Contacto contacto) {
    // Eliminamos el contacto de la lista de contactos.
    _contactos.remove(contacto);
  }

  // Añadimos un método llamado 'buscarContacto' que nos permitirá buscar un contacto en la agenda.
  Contacto? buscarContacto(String nombre) {
    // Recorremos la lista de contactos.
    for (Contacto contacto in _contactos) {
      // Si el nombre del contacto coincide con el nombre que estamos buscando, devolvemos el contacto.
      if (contacto.nombre == nombre) {
        return contacto;
      }
    }

    // Si no encontramos el contacto, devolvemos 'null'.
    return null;
  }

  // Añadimos un método llamado 'imprimirAgenda' que nos permitirá imprimir la agenda en la consola.
  void imprimirAgenda() {
    // Recorremos la lista de contactos.
    for (Contacto contacto in _contactos) {
      // Imprimimos el nombre y el teléfono del contacto.
      print('${contacto.nombre}: ${contacto.telefono}');
    }
  }
}

// Definimos una clase llamada 'Contacto' que nos permitirá representar un contacto de la agenda.
class Contacto {
  // Creamos una propiedad llamada 'nombre' que almacenará el nombre del contacto.
  String nombre;

  // Creamos una propiedad llamada 'telefono' que almacenará el teléfono del contacto.
  String telefono;

  // Creamos un constructor que nos permitirá inicializar el nombre y el teléfono del contacto.
  Contacto(this.nombre, this.telefono);
}

// Creamos una función principal llamada 'main' que nos permitirá ejecutar el programa.
void main() {
  // Creamos una instancia de la clase 'Agenda'.
  Agenda agenda = Agenda();

  // Creamos algunos contactos.
  Contacto contacto1 = Contacto('Juan', '123-456-789');
  Contacto contacto2 = Contacto('María', '987-654-321');
  Contacto contacto3 = Contacto('Pedro', '456-789-123');

  // Añadimos los contactos a la agenda.
  agenda.agregarContacto(contacto1);
  agenda.agregarContacto(contacto2);
  agenda.agregarContacto(contacto3);

  // Imprimimos la agenda en la consola.
  agenda.imprimirAgenda();

  // Buscamos un contacto en la agenda.
  Contacto? contactoBuscado = agenda.buscarContacto('María');

  // Imprimimos el nombre y el teléfono del contacto buscado.
  print('Contacto buscado: ${contactoBuscado?.nombre} - ${contactoBuscado?.telefono}');

  // Eliminamos un contacto de la agenda.
  agenda.eliminarContacto(contacto1);

  // Imprimimos la agenda en la consola.
  agenda.imprimirAgenda();
}
```

Este código crea una agenda de contactos simple que nos permite añadir, eliminar y buscar contactos, así como imprimir la agenda en la consola. El código está dividido en tres clases: `Agenda`, `Contacto` y `Main`.

La clase `Agenda` es la clase principal que se encarga de gestionar la agenda de contactos. Tiene tres métodos: `agregarContacto`, `eliminarContacto` y `buscarContacto`. El método `agregarContacto` añade un nuevo contacto a la lista de contactos, el método `eliminarContacto` elimina un contacto de la lista de contactos y el método `buscarContacto` devuelve el contacto cuyo nombre coincide con el nombre especificado.

La clase `Contacto` es una clase simple que representa un contacto de la agenda. Tiene dos propiedades: `nombre` y `telefono`.

La función `main` es la función principal del programa. Crea una instancia de la clase `Agenda`, añade algunos contactos a la agenda, imprime la agenda en la consola, busca un contacto en la agenda, elimina un contacto de la agenda y vuelve a imprimir la agenda en la consola.