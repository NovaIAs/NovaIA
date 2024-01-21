```dart
// Definición de la clase Persona
class Persona {
  String nombre;
  int edad;
  Persona(this.nombre, this.edad);

  String saludar() {
    return "Hola, mi nombre es $nombre y tengo $edad años.";
  }
}

// Definición de la clase Estudiante, una subclase de Persona
class Estudiante extends Persona {
  String carrera;
  Estudiante(String nombre, int edad, this.carrera) : super(nombre, edad);

  @override
  String saludar() {
    return "Hola, soy ${super.nombre}, estudio $carrera y tengo ${super.edad} años.";
  }
}

// Definición de la clase Profesor, una subclase de Persona
class Profesor extends Persona {
  String materia;
  Profesor(String nombre, int edad, this.materia) : super(nombre, edad);

  @override
  String saludar() {
    return "Hola, soy ${super.nombre}, enseño $materia y tengo ${super.edad} años.";
  }
}

// Función principal
void main() {
  // Crear un objeto de la clase Persona
  var persona1 = Persona("Juan", 20);

  // Crear un objeto de la clase Estudiante
  var estudiante1 = Estudiante("María", 18, "Ingeniería");

  // Crear un objeto de la clase Profesor
  var profesor1 = Profesor("Pedro", 40, "Matemáticas");

  // Imprimir el saludo de cada objeto
  print(persona1.saludar());
  print(estudiante1.saludar());
  print(profesor1.saludar());
}
```

Explicación del código:

1. **Definición de clases:** Se definen tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` es la clase base y las clases `Estudiante` y `Profesor` son clases derivadas.

2. **Atributos:** Cada clase tiene sus propios atributos. La clase `Persona` tiene los atributos `nombre` y `edad`. La clase `Estudiante` tiene el atributo adicional `carrera`. La clase `Profesor` tiene el atributo adicional `materia`.

3. **Constructores:** Cada clase tiene su propio constructor, que se utiliza para inicializar los atributos de la clase.

4. **Método saludar:** Cada clase tiene un método `saludar` que imprime un mensaje diferente dependiendo de la clase.

5. **Función principal:** La función `main` se ejecuta cuando se inicia el programa. En esta función se crean objetos de las clases `Persona`, `Estudiante` y `Profesor` y se imprime el saludo de cada objeto.