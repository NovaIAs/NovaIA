```groovy
// Definición de una clase abstracta "Persona" con propiedades y métodos básicos
abstract class Persona {
   String nombre
   int edad
   String ocupacion

   // Constructor de la clase
   Persona(String nombre, int edad, String ocupacion) {
      this.nombre = nombre
      this.edad = edad
      this.ocupacion = ocupacion
   }

   // Método abstracto "hablar" que cada subclase debe implementar
   abstract String hablar()
}

// Definición de una interfaz "Empleado" con un método abstracto
interface Empleado {
   int calcularSalario()
}

// Definición de la clase "Ingeniero" que hereda de "Persona" e implementa "Empleado"
class Ingeniero extends Persona implements Empleado {
   String especialidad

   // Constructor de la clase
   Ingeniero(String nombre, int edad, String ocupacion, String especialidad) {
      super(nombre, edad, ocupacion)  // Llama al constructor de la clase padre
      this.especialidad = especialidad
   }

   // Implementación del método "hablar"
   @Override
   String hablar() {
      return "Hola, soy un ingeniero llamado $nombre y mi especialidad es $especialidad."
   }

   // Implementación del método "calcularSalario"
   @Override
   int calcularSalario() {
      return 50000  // Salario fijo para los ingenieros
   }
}

// Definición de la clase "Doctor" que hereda de "Persona" e implementa "Empleado"
class Doctor extends Persona implements Empleado {
   String especialidad

   // Constructor de la clase
   Doctor(String nombre, int edad, String ocupacion, String especialidad) {
      super(nombre, edad, ocupacion)  // Llama al constructor de la clase padre
      this.especialidad = especialidad
   }

   // Implementación del método "hablar"
   @Override
   String hablar() {
      return "Hola, soy un doctor llamado $nombre y mi especialidad es $especialidad."
   }

   // Implementación del método "calcularSalario"
   @Override
   int calcularSalario() {
      return 60000  // Salario fijo para los doctores
   }
}

// Definición de la clase "Profesor" que hereda de "Persona" e implementa "Empleado"
class Profesor extends Persona implements Empleado {
   String materia

   // Constructor de la clase
   Profesor(String nombre, int edad, String ocupacion, String materia) {
      super(nombre, edad, ocupacion)  // Llama al constructor de la clase padre
      this.materia = materia
   }

   // Implementación del método "hablar"
   @Override
   String hablar() {
      return "Hola, soy un profesor llamado $nombre y enseño $materia."
   }

   // Implementación del método "calcularSalario"
   @Override
   int calcularSalario() {
      return 40000  // Salario fijo para los profesores
   }
}

// Creación de una lista de personas
List<Persona> personas = [
   new Ingeniero("Juan", 30, "Ingeniero de Software", "Java"),
   new Doctor("María", 35, "Doctora", "Cardiología"),
   new Profesor("Pedro", 40, "Profesor", "Matemáticas")
]

// Impresión de la información de cada persona
personas.each { persona ->
   println persona.hablar()
   println "Salario: ${persona.calcularSalario()}"
}
```

Explicación del código:

1. **Definición de clases y abstracción:** Se definen tres clases: `Persona`, `Empleado` e `Ingeniero`, `Doctor` y `Profesor`. La clase `Persona` es abstracta, lo que significa que no se puede instanciar directamente, sino que sirve como base para crear subclases. Las clases `Ingeniero`, `Doctor` y `Profesor` heredan de `Persona` y añaden propiedades y comportamientos específicos. La interfaz `Empleado` define un método abstracto `calcularSalario()`, que debe ser implementado por las subclases que la implementan.

2. **Polimorfismo:** El uso de herencia y polimorfismo permite que las subclases compartan comportamientos comunes definidos en la clase padre, pero también tengan su propia implementación personalizada de métodos. Por ejemplo, las clases `Ingeniero`, `Doctor` y `Profesor` tienen su propia implementación del método `hablar()`, que devuelve una descripción específica de cada profesión.

3. **Creación de objetos:** Se crea una lista de personas que contiene instancias de las clases `Ingeniero`, `Doctor` y `Profesor`.

4. **Iteración y procesamiento de objetos:** Se utiliza el método `each()` de la lista para iterar sobre cada persona y realizar ciertas acciones, como imprimir su información y calcular su salario.

5. **Uso de closures:** Se utiliza un closure (bloque de código encerrado entre llaves) para definir el comportamiento de cada persona al hablar. El closure tiene acceso a las propiedades y métodos de la persona actual dentro del bucle.

6. **Impresión de información:** Se utiliza la función `println` para imprimir la información de cada persona en la consola.