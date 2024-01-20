```java

// Importar las bibliotecas necesarias
import java.util.Scanner;
import java.util.ArrayList;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class Complejo {

    // Definir una clase Persona
    private static class Persona {
        String nombre;
        int edad;
        String ocupacion;

        Persona(String nombre, int edad, String ocupacion) {
            this.nombre = nombre;
            this.edad = edad;
            this.ocupacion = ocupacion;
        }

        @Override
        public String toString() {
            return nombre + ", " + edad + ", " + ocupacion;
        }
    }

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        // Crear una lista de personas
        ArrayList<Persona> personas = new ArrayList<Persona>();

        // Leer los datos de las personas desde la consola
        System.out.println("Introduzca los datos de las personas (nombre, edad y ocupación):");
        while (true) {
            String line = scanner.nextLine();
            if (line.equals("")) {
                break;
            }
            String[] datos = line.split(",");
            personas.add(new Persona(datos[0], Integer.parseInt(datos[1]), datos[2]));
        }

        // Escribir los datos de las personas a un archivo de texto
        File file = new File("personas.txt");
        FileWriter writer = new FileWriter(file);
        for (Persona persona : personas) {
            writer.write(persona.toString() + "\n");
        }
        writer.close();

        // Leer los datos de las personas desde el archivo de texto
        ArrayList<Persona> personas2 = new ArrayList<Persona>();
        Scanner scanner2 = new Scanner(file);
        while (scanner2.hasNextLine()) {
            String line2 = scanner2.nextLine();
            String[] datos2 = line2.split(",");
            personas2.add(new Persona(datos2[0], Integer.parseInt(datos2[1]), datos2[2]));
        }
        scanner2.close();

        // Mostrar los datos de las personas leídas desde el archivo de texto
        System.out.println("Personas leídas desde el archivo de texto:");
        for (Persona persona2 : personas2) {
            System.out.println(persona2.toString());
        }
    }
}

```

Este código en JAVA es complejo ya que realiza las siguientes tareas:

* Crea una clase Persona con sus atributos: nombre, edad y ocupación.
* Crea una lista de personas.
* Lee los datos de las personas desde la consola.
* Escribe los datos de las personas a un archivo de texto.
* Lee los datos de las personas desde el archivo de texto.
* Muestra los datos de las personas leídas desde el archivo de texto.

Este código es muy amplio y diferenciado, por lo que es difícil que se repita nuevamente. Además, el código está en español, lo que lo hace más fácil de entender.