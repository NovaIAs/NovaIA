```dart
// Definición de una clase Persona
class Persona {
  String nombre;
  int edad;
  List<String> habilidades;

  // Constructor de la clase Persona
  Persona({required this.nombre, required this.edad, required this.habilidades});

  // Método para obtener el nombre de la persona
  String obtenerNombre() {
    return nombre;
  }

  // Método para obtener la edad de la persona
  int obtenerEdad() {
    return edad;
  }

  // Método para obtener las habilidades de la persona
  List<String> obtenerHabilidades() {
    return habilidades;
  }

  // Método para agregar una nueva habilidad a la lista de habilidades de la persona
  void agregarHabilidad(String nuevaHabilidad) {
    habilidades.add(nuevaHabilidad);
  }

  // Método para eliminar una habilidad de la lista de habilidades de la persona
  void eliminarHabilidad(String habilidadAEliminar) {
    habilidades.remove(habilidadAEliminar);
  }

  // Método para imprimir la información de la persona
  void imprimirInformacion() {
    print("Nombre: $nombre");
    print("Edad: $edad");
    print("Habilidades:");
    for (String habilidad in habilidades) {
      print("- $habilidad");
    }
  }
}

// Definición de una clase Empleado que hereda de la clase Persona
class Empleado extends Persona {
  String cargo;
  double salario;

  // Constructor de la clase Empleado
  Empleado({required String nombre, required int edad, required List<String> habilidades, required this.cargo, required this.salario}) : super(nombre: nombre, edad: edad, habilidades: habilidades);

  // Método para obtener el cargo del empleado
  String obtenerCargo() {
    return cargo;
  }

  // Método para obtener el salario del empleado
  double obtenerSalario() {
    return salario;
  }

  // Método para imprimir la información del empleado
  @override
  void imprimirInformacion() {
    super.imprimirInformacion();
    print("Cargo: $cargo");
    print("Salario: $salario");
  }
}

// Definición de una clase Empresa
class Empresa {
  String nombre;
  List<Empleado> empleados;

  // Constructor de la clase Empresa
  Empresa({required this.nombre, required this.empleados});

  // Método para obtener el nombre de la empresa
  String obtenerNombre() {
    return nombre;
  }

  // Método para obtener la lista de empleados de la empresa
  List<Empleado> obtenerEmpleados() {
    return empleados;
  }

  // Método para agregar un nuevo empleado a la lista de empleados de la empresa
  void agregarEmpleado(Empleado nuevoEmpleado) {
    empleados.add(nuevoEmpleado);
  }

  // Método para eliminar un empleado de la lista de empleados de la empresa
  void eliminarEmpleado(Empleado empleadoAEliminar) {
    empleados.remove(empleadoAEliminar);
  }

  // Método para imprimir la información de la empresa
  void imprimirInformacion() {
    print("Nombre: $nombre");
    print("Empleados:");
    for (Empleado empleado in empleados) {
      empleado.imprimirInformacion();
      print("");
    }
  }
}

// Función principal del programa
void main() {
  // Creación de un objeto de la clase Persona
  Persona persona1 = Persona(nombre: "Juan", edad: 25, habilidades: ["Programar", "Diseñar", "Tocar la guitarra"]);

  // Creación de un objeto de la clase Empleado
  Empleado empleado1 = Empleado(nombre: "María", edad: 30, habilidades: ["Programar", "Administrar", "Hablar inglés"], cargo: "Gerente", salario: 100000);

  // Creación de un objeto de la clase Empresa
  Empresa empresa1 = Empresa(nombre: "Acme Corporation", empleados: [empleado1]);

  // Impresión de la información de la persona
  print("Información de la persona:");
  persona1.imprimirInformacion();
  print("");

  // Impresión de la información del empleado
  print("Información del empleado:");
  empleado1.imprimirInformacion();
  print("");

  // Impresión de la información de la empresa
  print("Información de la empresa:");
  empresa1.imprimirInformacion();
}
```

Este código crea una clase `Persona` con atributos para el nombre, la edad y las habilidades. También incluye métodos para obtener y modificar estos atributos, así como un método para imprimir la información de la persona.

La clase `Empleado` hereda de la clase `Persona` y añade atributos para el cargo y el salario. También incluye métodos para obtener y modificar estos atributos, así como un método para imprimir la información del empleado.

La clase `Empresa` tiene atributos para el nombre y la lista de empleados. También incluye métodos para obtener y modificar estos atributos, así como un método para imprimir la información de la empresa.

En la función `main()`, se crean objetos de las clases `Persona`, `Empleado` y `Empresa`, y se imprimen sus respectivos datos.

Este código es un ejemplo de cómo crear clases en Dart y cómo usar la herencia para crear nuevas clases a partir de clases existentes. También muestra cómo usar métodos para obtener y modificar los atributos de los objetos, y cómo imprimir la información de los objetos.