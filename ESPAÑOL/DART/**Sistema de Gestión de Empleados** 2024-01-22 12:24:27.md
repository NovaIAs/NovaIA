```
// Definición de una clase Persona con atributos nombre y edad
class Persona {
  String nombre;
  int edad;

  // Constructor de la clase Persona
  Persona(this.nombre, this.edad);

  // Método para obtener el nombre de la persona
  String getNombre() {
    return nombre;
  }

  // Método para obtener la edad de la persona
  int getEdad() {
    return edad;
  }

  // Método para incrementar la edad de la persona en un año
  void incrementarEdad() {
    edad++;
  }

  // Método para imprimir la información de la persona
  void imprimirInformacion() {
    print('Nombre: $nombre, Edad: $edad');
  }
}

// Definición de una clase Empleado que hereda de la clase Persona y añade el atributo salario
class Empleado extends Persona {
  double salario;

  // Constructor de la clase Empleado
  Empleado(String nombre, int edad, this.salario) : super(nombre, edad);

  // Método para obtener el salario del empleado
  double getSalario() {
    return salario;
  }

  // Método para incrementar el salario del empleado en un 10%
  void incrementarSalario() {
    salario *= 1.1;
  }

  // Método para imprimir la información del empleado
  @override
  void imprimirInformacion() {
    super.imprimirInformacion();
    print('Salario: $salario');
  }
}

// Definición de una clase Empresa que contiene una lista de empleados
class Empresa {
  List<Empleado> empleados;

  // Constructor de la clase Empresa
  Empresa(this.empleados);

  // Método para añadir un empleado a la lista
  void añadirEmpleado(Empleado empleado) {
    empleados.add(empleado);
  }

  // Método para obtener la lista de empleados
  List<Empleado> getEmpleados() {
    return empleados;
  }

  // Método para imprimir la información de todos los empleados
  void imprimirInformacionEmpleados() {
    for (Empleado empleado in empleados) {
      empleado.imprimirInformacion();
      print('\n');
    }
  }
}

// Función principal del programa
void main() {
  // Creación de una lista de empleados
  List<Empleado> empleados = [
    Empleado('Juan', 30, 1000.0),
    Empleado('María', 25, 1200.0),
    Empleado('Pedro', 40, 1500.0)
  ];

  // Creación de una empresa con la lista de empleados
  Empresa empresa = Empresa(empleados);

  // Añadir un nuevo empleado a la empresa
  empresa.añadirEmpleado(Empleado('Ana', 28, 1300.0));

  // Imprimir la información de todos los empleados de la empresa
  empresa.imprimirInformacionEmpleados();
}
```

Este código crea una clase `Persona` con los atributos `nombre` y `edad`, y una clase `Empleado` que hereda de `Persona` y añade el atributo `salario`.

Luego, se crea una lista de empleados y se pasa como argumento a una instancia de la clase `Empresa`.

La instancia de la clase `Empresa` se usa para añadir un nuevo empleado a la lista y para imprimir la información de todos los empleados de la empresa.

El código también incluye una función `main()` que se ejecuta cuando se ejecuta el programa.

En la función `main()` se crea la lista de empleados, se crea la instancia de la clase `Empresa` y se imprime la información de todos los empleados de la empresa.