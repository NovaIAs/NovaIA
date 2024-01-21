```dart
// Creamos una clase llamada "Coche" que representa un vehículo.

class Coche {
  // Esta clase tiene dos propiedades: "nombre" y "velocidad".
  String nombre;
  int velocidad;

  // Constructor de la clase "Coche".
  Coche(this.nombre, this.velocidad);

  // Método para imprimir el nombre y la velocidad del coche.
  void imprimir() {
    print("Nombre: $nombre, Velocidad: $velocidad");
  }
}

// Creamos una clase llamada "Garaje" que representa un lugar para almacenar coches.

class Garaje {
  // Esta clase tiene una propiedad llamada "coches" que es una lista de coches.
  List<Coche> coches;

  // Constructor de la clase "Garaje".
  Garaje(this.coches);

  // Método para imprimir la lista de coches almacenados en el garaje.
  void imprimir() {
    for (Coche coche in coches) {
      coche.imprimir();
    }
  }
}

// Creamos una función llamada "crearCoche" que devuelve una instancia de la clase "Coche".

Coche crearCoche(String nombre, int velocidad) {
  return Coche(nombre, velocidad);
}

// Creamos una función llamada "crearGaraje" que devuelve una instancia de la clase "Garaje".

Garaje crearGaraje(List<Coche> coches) {
  return Garaje(coches);
}

// Creamos una función principal.

void main() {
  // Creamos una lista de coches.
  List<Coche> coches = [
    crearCoche("Coche 1", 100),
    crearCoche("Coche 2", 120),
    crearCoche("Coche 3", 150),
  ];

  // Creamos un garaje con la lista de coches.
  Garaje garaje = crearGaraje(coches);

  // Imprimimos la lista de coches almacenados en el garaje.
  garaje.imprimir();
}
```

Explicación:

1. **Clases:** Definimos dos clases: "Coche" y "Garaje". La clase "Coche" representa un vehículo con propiedades como el nombre y la velocidad, mientras que la clase "Garaje" representa un lugar para almacenar coches con una propiedad que es una lista de coches.

2. **Constructores:** Cada clase tiene un constructor que inicializa sus propiedades.

3. **Métodos:** Ambas clases tienen un método llamado "imprimir" que imprime la información de los coches o del garaje, respectivamente.

4. **Funciones:** Definimos dos funciones: "crearCoche" y "crearGaraje" que devuelven instancias de las clases "Coche" y "Garaje", respectivamente.

5. **Función principal:** La función principal crea una lista de coches, crea un garaje con la lista de coches y luego imprime la lista de coches almacenados en el garaje.