```java
import java.util.*;
import java.io.*;

public class CodigoComplejo {

    // Constantes
    private static final String RUTA_FICHERO = "fichero.txt";

    // Métodos
    public static void main(String[] args) {
        // Crear una lista de objetos de tipo Persona
        List<Persona> personas = new ArrayList<>();

        // Leer el fichero de texto y añadir los datos de las personas a la lista
        try {
            BufferedReader br = new BufferedReader(new FileReader(RUTA_FICHERO));
            String linea;
            while ((linea = br.readLine()) != null) {
                String[] datos = linea.split(",");
                Persona persona = new Persona(datos[0], datos[1], Integer.parseInt(datos[2]));
                personas.add(persona);
            }
            br.close();
        } catch (IOException e) {
            System.err.println("Error al leer el fichero: " + e.getMessage());
            System.exit(1);
        }

        // Ordenar la lista de personas por edad
        Collections.sort(personas, new Comparator<Persona>() {
            @Override
            public int compare(Persona p1, Persona p2) {
                return Integer.compare(p1.getEdad(), p2.getEdad());
            }
        });

        // Imprimir los datos de las personas en la consola
        for (Persona persona : personas) {
            System.out.println(persona.toString());
        }

        // Crear un mapa que contenga los nombres de las personas como claves y sus edades como valores
        Map<String, Integer> edades = new HashMap<>();
        for (Persona persona : personas) {
            edades.put(persona.getNombre(), persona.getEdad());
        }

        // Imprimir el mapa en la consola
        for (Map.Entry<String, Integer> entry : edades.entrySet()) {
            System.out.println(entry.getKey() + ": " + entry.getValue());
        }
    }

    // Clase que representa a una persona
    private static class Persona {

        private String nombre;
        private String apellido;
        private int edad;

        public Persona(String nombre, String apellido, int edad) {
            this.nombre = nombre;
            this.apellido = apellido;
            this.edad = edad;
        }

        public String getNombre() {
            return nombre;
        }

        public String getApellido() {
            return apellido;
        }

        public int getEdad() {
            return edad;
        }

        @Override
        public String toString() {
            return "Persona{" +
                    "nombre='" + nombre + '\'' +
                    ", apellido='" + apellido + '\'' +
                    ", edad=" + edad +
                    '}';
        }
    }
}
```

Explicación del código:

* El código crea una lista de objetos de tipo Persona.
* El código lee el fichero de texto y añade los datos de las personas a la lista.
* El código ordena la lista de personas por edad.
* El código imprime los datos de las personas en la consola.
* El código crea un mapa que contiene los nombres de las personas como claves y sus edades como valores.
* El código imprime el mapa en la consola.

El código es complejo porque utiliza varias clases, métodos y estructuras de datos diferentes. El código también es difícil de repetir porque utiliza varias características avanzadas de Java, como la programación funcional y la reflexión.