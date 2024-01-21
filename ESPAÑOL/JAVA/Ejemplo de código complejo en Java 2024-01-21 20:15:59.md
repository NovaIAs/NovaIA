```java
import java.util.*;
import java.lang.*;
import java.io.*;

public class CodigoComplejo {

    public static void main(String[] args) {
        // Crear un ArrayList de cadenas
        ArrayList<String> cadenas = new ArrayList<>();
        // Añadir algunas cadenas al ArrayList
        cadenas.add("Hola");
        cadenas.add("mundo");
        cadenas.add("!");

        // Crear un HashMap de claves y valores enteros
        HashMap<Integer, Integer> mapa = new HashMap<>();
        // Añadir algunos pares clave-valor al HashMap
        mapa.put(1, 10);
        mapa.put(2, 20);
        mapa.put(3, 30);

        // Crear un bucle for-each para iterar sobre el ArrayList de cadenas
        for (String cadena : cadenas) {
            // Imprimir cada cadena en la consola
            System.out.println(cadena);
        }

        // Crear un bucle for-each para iterar sobre el HashMap de claves y valores enteros
        for (Map.Entry<Integer, Integer> entrada : mapa.entrySet()) {
            // Imprimir cada par clave-valor en la consola
            System.out.println(entrada.getKey() + " " + entrada.getValue());
        }

        // Crear una clase interna anónima que implementa la interfaz Runnable
        Runnable tarea = new Runnable() {
            @Override
            public void run() {
                // Código que se ejecutará en un hilo aparte
                System.out.println("Hola desde un hilo aparte!");
            }
        };

        // Crear un nuevo hilo y ejecutar la tarea en ese hilo
        Thread hilo = new Thread(tarea);
        hilo.start();

        // Esperar a que el hilo termine de ejecutarse
        try {
            hilo.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // Crear una clase genérica que pueda almacenar cualquier tipo de dato
        class Caja<T> {
            private T contenido;

            public Caja(T contenido) {
                this.contenido = contenido;
            }

            public T getContenido() {
                return contenido;
            }

            public void setContenido(T contenido) {
                this.contenido = contenido;
            }
        }

        // Crear una instancia de la clase Caja y almacenar una cadena en ella
        Caja<String> caja = new Caja<>("Hola mundo!");

        // Obtener el contenido de la caja y mostrarlo en la consola
        String contenido = caja.getContenido();
        System.out.println(contenido);

        // Crear una clase que implemente la interfaz Comparator
        class ComparadorEnteros implements Comparator<Integer> {
            @Override
            public int compare(Integer o1, Integer o2) {
                return o1.compareTo(o2);
            }
        }

        // Crear una lista de enteros
        List<Integer> lista = new ArrayList<>();
        // Añadir algunos enteros a la lista
        lista.add(1);
        lista.add(3);
        lista.add(2);

        // Ordenar la lista utilizando el comparador
        Collections.sort(lista, new ComparadorEnteros());

        // Imprimir la lista ordenada en la consola
        for (Integer entero : lista) {
            System.out.println(entero);
        }
    }
}
```

Explicación del código:

* El código crea un ArrayList de cadenas y lo rellena con algunas cadenas.
* Crea un HashMap de claves y valores enteros y lo rellena con algunos pares clave-valor.
* Crea un bucle for-each para iterar sobre el ArrayList de cadenas y lo imprime en la consola.
* Crea un bucle for-each para iterar sobre el HashMap de claves y valores enteros y lo imprime en la consola.
* Crea una clase interna anónima que implementa la interfaz Runnable y la ejecuta en un hilo aparte.
* Crea una clase genérica que puede almacenar cualquier tipo de dato y la instancia con una cadena.
* Crea una clase que implementa la interfaz Comparator y la utiliza para ordenar una lista de enteros.
* Imprime la lista ordenada en la consola.