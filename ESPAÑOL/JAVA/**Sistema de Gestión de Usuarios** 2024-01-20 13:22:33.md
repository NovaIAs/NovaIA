```java
// Importación de las librerías necesarias
import java.util.*;
import java.io.*;

public class SistemaComplejo {

    // Clase principal del programa
    public static void main(String[] args) {
        // Creación de un objeto Scanner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Creación de una lista de cadenas de caracteres para almacenar los nombres de los usuarios
        List<String> nombresUsuarios = new ArrayList<>();

        // Creación de una lista de objetos de la clase Usuario para almacenar los datos de los usuarios
        List<Usuario> usuarios = new ArrayList<>();

        // Bucle para leer los datos de los usuarios
        while (true) {
            // Solicita al usuario que introduzca su nombre
            System.out.println("Introduzca su nombre:");

            // Lee el nombre del usuario
            String nombreUsuario = scanner.nextLine();

            // Si el usuario introduce una cadena vacía, se sale del bucle
            if (nombreUsuario.isEmpty()) {
                break;
            }

            // Añade el nombre del usuario a la lista de nombres de usuarios
            nombresUsuarios.add(nombreUsuario);

            // Creación de un objeto de la clase Usuario
            Usuario usuario = new Usuario();

            // Asignación del nombre del usuario al objeto de la clase Usuario
            usuario.setNombre(nombreUsuario);

            // Solicita al usuario que introduzca su edad
            System.out.println("Introduzca su edad:");

            // Lee la edad del usuario
            int edadUsuario = Integer.parseInt(scanner.nextLine());

            // Asignación de la edad del usuario al objeto de la clase Usuario
            usuario.setEdad(edadUsuario);

            // Añade el objeto de la clase Usuario a la lista de usuarios
            usuarios.add(usuario);
        }

        // Bucle para imprimir los datos de los usuarios
        for (Usuario usuario : usuarios) {
            // Imprime el nombre del usuario
            System.out.println("Nombre: " + usuario.getNombre());

            // Imprime la edad del usuario
            System.out.println("Edad: " + usuario.getEdad());
        }

        // Creación de un objeto de la clase File para almacenar el archivo de salida
        File archivoSalida = new File("salida.txt");

        // Creación de un objeto de la clase FileWriter para escribir en el archivo de salida
        FileWriter escritorArchivo = null;

        try {
            // Inicialización del objeto de la clase FileWriter
            escritorArchivo = new FileWriter(archivoSalida);

            // Bucle para escribir los datos de los usuarios en el archivo de salida
            for (Usuario usuario : usuarios) {
                // Escribe el nombre del usuario en el archivo de salida
                escritorArchivo.write(usuario.getNombre() + "\n");

                // Escribe la edad del usuario en el archivo de salida
                escritorArchivo.write(usuario.getEdad() + "\n");
            }

            // Cierra el objeto de la clase FileWriter
            escritorArchivo.close();
        } catch (IOException e) {
            // Manejo de la excepción de entrada/salida
            System.err.println("Error al escribir en el archivo de salida.");
            e.printStackTrace();
        }
    }

    // Clase que representa a un usuario
    private static class Usuario {
        // Propiedades de la clase Usuario
        private String nombre;
        private int edad;

        // Constructor de la clase Usuario
        public Usuario() {
            this.nombre = "";
            this.edad = 0;
        }

        // Métodos de acceso a las propiedades de la clase Usuario
        public String getNombre() {
            return nombre;
        }

        public void setNombre(String nombre) {
            this.nombre = nombre;
        }

        public int getEdad() {
            return edad;
        }

        public void setEdad(int edad) {
            this.edad = edad;
        }
    }
}
```

Este código es un programa que lee los datos de los usuarios y los almacena en una lista de objetos de la clase Usuario. A continuación, imprime los datos de los usuarios por consola y los escribe en un archivo de texto.

El código está dividido en varias clases:

* La clase `SistemaComplejo` es la clase principal del programa. Contiene el método `main()` que se ejecuta cuando se ejecuta el programa.
* La clase `Usuario` representa a un usuario. Contiene las propiedades `nombre` y `edad`.
* La clase `Scanner` se utiliza para leer la entrada del usuario.
* La clase `FileWriter` se utiliza para escribir en un archivo de texto.

El programa funciona de la siguiente manera:

1. Se crea una lista de cadenas de caracteres para almacenar los nombres de los usuarios.
2. Se crea una lista de objetos de la clase Usuario para almacenar los datos de los usuarios.
3. Se entra en un bucle que solicita al usuario que introduzca su nombre y edad.
4. Se añade el nombre del usuario a la lista de nombres de usuarios y se crea un objeto de la clase Usuario.
5. Se asigna el nombre y la edad del usuario al objeto de la clase Usuario.
6. Se añade el objeto de la clase Usuario a la lista de usuarios.
7. Se sale del bucle cuando el usuario introduce una cadena vacía.
8. Se imprime la información de los usuarios en consola.
9. Se crea un objeto de la clase File para almacenar el archivo de salida.
10. Se crea un objeto de la clase FileWriter para escribir en el archivo de salida.
11. Se entra en un bucle que escribe los datos de los usuarios en el archivo de salida.
12. Se cierra el objeto de la clase FileWriter.

Este código es un ejemplo de cómo utilizar diferentes clases y objetos para crear un programa complejo.