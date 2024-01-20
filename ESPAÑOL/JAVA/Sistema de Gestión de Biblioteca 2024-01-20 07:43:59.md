```java
import java.util.Scanner;

public class SistemaDeGestionDeBiblioteca {

    private static Scanner entrada;

    public static void main(String[] args) {
        entrada = new Scanner(System.in);

        // Crear una biblioteca vacía
        Biblioteca biblioteca = new Biblioteca();

        // Bucle principal del programa
        while (true) {
            // Mostrar el menú de opciones
            System.out.println("Elija una opción:");
            System.out.println("1. Añadir un libro");
            System.out.println("2. Buscar un libro");
            System.out.println("3. Pedir un libro prestado");
            System.out.println("4. Devolver un libro");
            System.out.println("5. Salir");

            // Leer la opción elegida por el usuario
            int opcion = entrada.nextInt();

            // Ejecutar la opción elegida
            switch (opcion) {
                case 1:
                    añadirLibro(biblioteca);
                    break;
                case 2:
                    buscarLibro(biblioteca);
                    break;
                case 3:
                    pedirLibroPrestado(biblioteca);
                    break;
                case 4:
                    devolverLibro(biblioteca);
                    break;
                case 5:
                    System.out.println("Saliendo del programa...");
                    System.exit(0);
                default:
                    System.out.println("Opción no válida");
            }
        }
    }

    private static void añadirLibro(Biblioteca biblioteca) {
        // Pedir los datos del libro al usuario
        System.out.println("Introduzca el título del libro:");
        String titulo = entrada.nextLine();

        System.out.println("Introduzca el autor del libro:");
        String autor = entrada.nextLine();

        System.out.println("Introduzca el año de publicación del libro:");
        int añoPublicacion = entrada.nextInt();

        // Crear un nuevo libro con los datos introducidos por el usuario
        Libro libro = new Libro(titulo, autor, añoPublicacion);

        // Añadir el libro a la biblioteca
        biblioteca.añadirLibro(libro);

        // Mostrar un mensaje de confirmación al usuario
        System.out.println("El libro se ha añadido correctamente a la biblioteca");
    }

    private static void buscarLibro(Biblioteca biblioteca) {
        // Pedir el título del libro al usuario
        System.out.println("Introduzca el título del libro que desea buscar:");
        String titulo = entrada.nextLine();

        // Buscar el libro en la biblioteca
        Libro libro = biblioteca.buscarLibro(titulo);

        // Mostrar el libro encontrado al usuario, o un mensaje de error si no se ha encontrado
        if (libro != null) {
            System.out.println("Se ha encontrado el libro:");
            System.out.println("Título: " + libro.getTitulo());
            System.out.println("Autor: " + libro.getAutor());
            System.out.println("Año de publicación: " + libro.getAñoPublicacion());
        } else {
            System.out.println("No se ha encontrado el libro");
        }
    }

    private static void pedirLibroPrestado(Biblioteca biblioteca) {
        // Pedir el título del libro al usuario
        System.out.println("Introduzca el título del libro que desea pedir prestado:");
        String titulo = entrada.nextLine();

        // Buscar el libro en la biblioteca
        Libro libro = biblioteca.buscarLibro(titulo);

        // Pedir el libro prestado al usuario, o mostrar un mensaje de error si no se ha encontrado
        if (libro != null) {
            System.out.println("¿Está seguro de que desea pedir prestado el libro?");
            System.out.println("Título: " + libro.getTitulo());
            System.out.println("Autor: " + libro.getAutor());
            System.out.println("Año de publicación: " + libro.getAñoPublicacion());

            String respuesta = entrada.nextLine();

            if (respuesta.equals("sí")) {
                biblioteca.pedirLibroPrestado(libro);
                System.out.println("El libro se ha pedido prestado correctamente");
            } else {
                System.out.println("El libro no se ha pedido prestado");
            }
        } else {
            System.out.println("No se ha encontrado el libro");
        }
    }

    private static void devolverLibro(Biblioteca biblioteca) {
        // Pedir el título del libro al usuario
        System.out.println("Introduzca el título del libro que desea devolver:");
        String titulo = entrada.nextLine();

        // Buscar el libro en la biblioteca