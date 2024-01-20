```java
import java.util.*;
import java.util.stream.*;
import java.time.*;
import java.time.format.*;

public class Complejo {

  public static void main(String[] args) {
    // Crear una lista de números aleatorios
    List<Integer> numeros = new ArrayList<>();
    for (int i = 0; i < 100; i++) {
      numeros.add((int) (Math.random() * 100));
    }

    // Ordenar la lista de números
    numeros.sort(Comparator.naturalOrder());

    // Obtener el número más grande y el más pequeño de la lista
    int maximo = numeros.stream().max(Integer::compareTo).get();
    int minimo = numeros.stream().min(Integer::compareTo).get();

    // Calcular la media y la desviación estándar de la lista
    double media = numeros.stream().mapToInt(Integer::intValue).average().getAsDouble();
    double desviacionEstandar = Math.sqrt(numeros.stream().mapToInt(Integer::intValue).map(x -> Math.pow(x - media, 2)).sum() / (numeros.size() - 1));

    // Crear un mapa de los números pares e impares de la lista
    Map<String, List<Integer>> paresImpares = numeros.stream().collect(Collectors.partitioningBy(x -> x % 2 == 0));

    // Crear una cadena de texto con el resultado
    String resultado = String.format("Lista de números:\n%s\n\nNúmero máximo: %d\nNúmero mínimo: %d\nMedia: %.2f\nDesviación estándar: %.2f\n\nNúmeros pares:\n%s\nNúmeros impares:\n%s",
      numeros, maximo, minimo, media, desviacionEstandar, paresImpares.get("true"), paresImpares.get("false"));

    // Imprimir el resultado
    System.out.println(resultado);

    // Obtener la fecha y hora actual
    LocalDateTime ahora = LocalDateTime.now();

    // Formatear la fecha y hora en un formato legible
    DateTimeFormatter formato = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss");
    String fechaHora = ahora.format(formato);

    // Imprimir la fecha y hora actual
    System.out.println("\nFecha y hora actual: " + fechaHora);
  }
}
```

**Explicación del código:**

1. Se crea una lista de números aleatorios entre 0 y 99.

2. Se ordena la lista de números.

3. Se obtiene el número más grande y el más pequeño de la lista.

4. Se calcula la media y la desviación estándar de la lista.

5. Se crea un mapa que divide los números de la lista en pares e impares.

6. Se crea una cadena de texto con el resultado del análisis de la lista.

7. Se imprime el resultado.

8. Se obtiene la fecha y hora actual.

9. Se formatea la fecha y hora en un formato legible.

10. Se imprime la fecha y hora actuales.