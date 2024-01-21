```java
// Importamos las librerías necesarias
import java.util.Scanner;
import java.util.Random;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

public class CodigoComplejo {

    // Definimos una clase interna para representar un dato
    private static class Dato {
        private int valor; // Valor del dato
        private String etiqueta; // Etiqueta del dato

        public Dato(int valor, String etiqueta) {
            this.valor = valor;
            this.etiqueta = etiqueta;
        }

        public int getValor() {
            return valor;
        }

        public String getEtiqueta() {
            return etiqueta;
        }
    }

    public static void main(String[] args) {
        // Creamos un objeto Scanner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Creamos un objeto Random para generar números aleatorios
        Random random = new Random();

        // Creamos una lista de datos
        List<Dato> datos = new ArrayList<>();

        // Pedimos al usuario que introduzca una serie de datos
        System.out.println("Introduce una serie de datos separados por comas:");
        String input = scanner.nextLine();

        // Dividimos el input en una lista de strings
        String[] inputArray = input.split(",");

        // Iteramos sobre la lista de strings y añadimos los datos a la lista
        for (String s : inputArray) {
            String[] dato = s.split(":");
            datos.add(new Dato(Integer.parseInt(dato[0]), dato[1]));
        }

        // Ordenamos la lista de datos por valor
        datos.sort((a, b) -> Integer.compare(a.getValor(), b.getValor()));

        // Creamos una lista de etiquetas
        List<String> etiquetas = new ArrayList<>();

        // Iteramos sobre la lista de datos y añadimos las etiquetas a la lista
        for (Dato dato : datos) {
            etiquetas.add(dato.getEtiqueta());
        }

        // Imprimimos la lista de etiquetas
        System.out.println("Etiquetas:");
        for (String etiqueta : etiquetas) {
            System.out.println(etiqueta);
        }

        // Pedimos al usuario que introduzca un valor
        System.out.println("Introduce un valor:");
        int valor = scanner.nextInt();

        // Buscamos el dato con el valor más cercano al valor introducido por el usuario
        Dato datoMasCercano = null;
        int diferenciaMinima = Integer.MAX_VALUE;
        for (Dato dato : datos) {
            int diferencia = Math.abs(dato.getValor() - valor);
            if (diferencia < diferenciaMinima) {
                diferenciaMinima = diferencia;
                datoMasCercano = dato;
            }
        }

        // Imprimimos el dato más cercano
        System.out.println("Dato más cercano:");
        System.out.println(datoMasCercano.getValor() + ":" + datoMasCercano.getEtiqueta());

        // Generamos un número aleatorio entre 0 y el tamaño de la lista de datos
        int indiceAleatorio = random.nextInt(datos.size());

        // Obtenemos el dato en el índice aleatorio
        Dato datoAleatorio = datos.get(indiceAleatorio);

        // Imprimimos el dato aleatorio
        System.out.println("Dato aleatorio:");
        System.out.println(datoAleatorio.getValor() + ":" + datoAleatorio.getEtiqueta());

        // Cerramos el objeto Scanner
        scanner.close();
    }
}
```

Explicación:

Este código es un ejemplo de un código complejo y diferenciado en Java. El código realiza las siguientes tareas:

* Lee una serie de datos separados por comas introducidos por el usuario y los añade a una lista de datos.
* Ordena la lista de datos por valor.
* Crea una lista de etiquetas de los datos.
* Pide al usuario que introduzca un valor.
* Busca el dato con el valor más cercano al valor introducido por el usuario.
* Imprime el dato más cercano.
* Genera un número aleatorio entre 0 y el tamaño de la lista de datos.
* Obtiene el dato en el índice aleatorio.
* Imprime el dato aleatorio.

El código utiliza las siguientes técnicas de programación:

* Clases internas: La clase `Dato` es una clase interna de la clase `CodigoComplejo`. Esto significa que la clase `Dato` sólo puede ser utilizada dentro de la clase `CodigoComplejo`.
* Listas: El código utiliza listas para almacenar los datos y las etiquetas. Las listas son colecciones de objetos que se pueden recorrer y modificar.
* Ordenación: El código utiliza la función `sort()` para ordenar la lista de datos por valor. La función `sort()` utiliza un algoritmo de ordenación rápida.
* Búsqueda: El código utiliza un bucle `for` para buscar el dato con el valor más cercano al valor introducido por el usuario. El bucle `for` itera sobre la lista de datos y calcula la diferencia entre el valor del dato actual y el valor introducido por el usuario. El dato con la diferencia más pequeña es el dato más cercano.
* Generación de números aleatorios: El código utiliza la clase `Random` para generar un número aleatorio. El constructor de la clase `Random()` toma una semilla como argumento. La semilla es un número que se utiliza para inicializar el generador de números aleatorios. El generador de números aleatorios utiliza la semilla para generar una secuencia de números aleatorios.