```dart
// Declaramos una clase llamada "Persona" que tiene dos campos: "nombre" y "edad".
class Persona {
  String nombre;
  int edad;

  // Creamos un constructor para la clase "Persona" que recibe como parámetros el nombre y la edad de la persona.
  Persona(this.nombre, this.edad);

  // Creamos un método para la clase "Persona" llamado "hablar" que imprime el nombre y la edad de la persona en la consola.
  void hablar() {
    print("Mi nombre es $nombre y tengo $edad años.");
  }
}

// Creamos una lista de personas.
List<Persona> personas = [
  Persona("Juan", 20),
  Persona("Ana", 25),
  Persona("Pedro", 30),
];

// Recorremos la lista de personas y llamamos al método "hablar" para cada persona.
for (Persona persona in personas) {
  persona.hablar();
}

// Creamos una clase llamada "Coche" que tiene tres campos: "marca", "modelo" y "año".
class Coche {
  String marca;
  String modelo;
  int año;

  // Creamos un constructor para la clase "Coche" que recibe como parámetros la marca, el modelo y el año del coche.
  Coche(this.marca, this.modelo, this.año);

  // Creamos un método para la clase "Coche" llamado "arrancar" que imprime el mensaje "El coche ha arrancado" en la consola.
  void arrancar() {
    print("El coche ha arrancado.");
  }

  // Creamos un método para la clase "Coche" llamado "parar" que imprime el mensaje "El coche ha parado" en la consola.
  void parar() {
    print("El coche ha parado.");
  }
}

// Creamos una lista de coches.
List<Coche> coches = [
  Coche("Toyota", "Yaris", 2020),
  Coche("Honda", "Civic", 2021),
  Coche("Ford", "Mustang", 2022),
];

// Recorremos la lista de coches y llamamos a los métodos "arrancar" y "parar" para cada coche.
for (Coche coche in coches) {
  coche.arrancar();
  coche.parar();
}
```

Explicación del código:

* Declaramos una clase llamada "Persona" que tiene dos campos: "nombre" y "edad".
* Creamos un constructor para la clase "Persona" que recibe como parámetros el nombre y la edad de la persona.
* Creamos un método para la clase "Persona" llamado "hablar" que imprime el nombre y la edad de la persona en la consola.
* Creamos una lista de personas.
* Recorremos la lista de personas y llamamos al método "hablar" para cada persona.
* Creamos una clase llamada "Coche" que tiene tres campos: "marca", "modelo" y "año".
* Creamos un constructor para la clase "Coche" que recibe como parámetros la marca, el modelo y el año del coche.
* Creamos un método para la clase "Coche" llamado "arrancar" que imprime el mensaje "El coche ha arrancado" en la consola.
* Creamos un método para la clase "Coche" llamado "parar" que imprime el mensaje "El coche ha parado" en la consola.
* Creamos una lista de coches.
* Recorremos la lista de coches y llamamos a los métodos "arrancar" y "parar" para cada coche.