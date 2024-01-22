```dart
// Definir una clase Restaurante con sus propiedades y métodos

class Restaurante {
  String nombre;
  String direccion;
  List<String> tiposDeComida;
  List<Plato> menu;
  List<Mesa> mesas;

  Restaurante({
    required this.nombre,
    required this.direccion,
    this.tiposDeComida = const [],
    this.menu = const [],
    this.mesas = const [],
  });

  // Método para agregar un plato al menú

  void agregarPlato(Plato plato) {
    menu.add(plato);
  }

  // Método para agregar una mesa al restaurante

  void agregarMesa(Mesa mesa) {
    mesas.add(mesa);
  }

  // Método para imprimir el menú del restaurante

  void imprimirMenu() {
    print("Menú del restaurante ${nombre}:");
    for (var plato in menu) {
      print(plato.nombre);
    }
  }

  // Método para imprimir la lista de mesas del restaurante

  void imprimirMesas() {
    print("Mesas del restaurante ${nombre}:");
    for (var mesa in mesas) {
      print(mesa.numero);
    }
  }
}

// Definir una clase Plato con sus propiedades

class Plato {
  String nombre;
  double precio;

  Plato({required this.nombre, required this.precio});
}

// Definir una clase Mesa con sus propiedades

class Mesa {
  int numero;
  int capacidad;

  Mesa({required this.numero, required this.capacidad});
}

// Crear una instancia de la clase Restaurante

var restaurante = Restaurante(
  nombre: "El Rincón Argentino",
  direccion: "Calle Mayor, 123",
  tiposDeComida: ["Argentino", "Asador"],
  menu: [
    Plato(nombre: "Bife de chorizo", precio: 20.0),
    Plato(nombre: "Empanadas de carne", precio: 10.0),
    Plato(nombre: "Choripán", precio: 5.0),
  ],
  mesas: [
    Mesa(numero: 1, capacidad: 4),
    Mesa(numero: 2, capacidad: 6),
    Mesa(numero: 3, capacidad: 8),
  ],
);

// Imprimir el menú del restaurante

restaurante.imprimirMenu();

// Imprimir la lista de mesas del restaurante

restaurante.imprimirMesas();
```

Este código define tres clases: Restaurante, Plato y Mesa, con sus respectivas propiedades y métodos. Luego crea una instancia de la clase Restaurante llamada "El Rincón Argentino" con una dirección, tipos de comida, menú y mesas específicas. Finalmente, imprime el menú y la lista de mesas del restaurante.