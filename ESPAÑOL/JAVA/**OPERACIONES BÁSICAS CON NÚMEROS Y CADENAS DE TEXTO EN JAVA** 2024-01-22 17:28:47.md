```java
import java.util.*;
import java.math.*;
import java.text.*;

public class CodigoComplejo {

    public static void main(String[] args) {
        // Declaración de variables
        Scanner entrada = new Scanner(System.in);
        DecimalFormat df = new DecimalFormat("0.00");
        Random aleatorio = new Random();

        // Solicitar datos al usuario
        System.out.println("Ingrese un número entero:");
        int numeroEntero = entrada.nextInt();
        System.out.println("Ingrese un número decimal:");
        double numeroDecimal = entrada.nextDouble();
        System.out.println("Ingrese una cadena de texto:");
        String cadenaTexto = entrada.nextLine();

        // Generar un número aleatorio
        int numeroAleatorio = aleatorio.nextInt(100);

        // Operaciones matemáticas
        int suma = numeroEntero + numeroAleatorio;
        double resta = numeroDecimal - numeroAleatorio;
        double multiplicacion = numeroDecimal * numeroAleatorio;
        double division = numeroDecimal / numeroAleatorio;
        int modulo = numeroEntero % numeroAleatorio;

        // Operaciones con cadenas de texto
        String concatenacion = cadenaTexto + " concatenada";
        String mayusculas = cadenaTexto.toUpperCase();
        String minusculas = cadenaTexto.toLowerCase();
        int longitud = cadenaTexto.length();
        char caracter = cadenaTexto.charAt(0);

        // Formateado de números
        String numeroEnteroFormateado = df.format(numeroEntero);
        String numeroDecimalFormateado = df.format(numeroDecimal);

        // Impresión de resultados
        System.out.println("Número entero: " + numeroEntero);
        System.out.println("Número decimal: " + numeroDecimal);
        System.out.println("Cadena de texto: " + cadenaTexto);
        System.out.println("Número aleatorio: " + numeroAleatorio);
        System.out.println("Suma: " + suma);
        System.out.println("Resta: " + resta);
        System.out.println("Multiplicación: " + multiplicacion);
        System.out.println("División: " + division);
        System.out.println("Módulo: " + modulo);
        System.out.println("Concatenación: " + concatenacion);
        System.out.println("Mayúsculas: " + mayusculas);
        System.out.println("Minúsculas: " + minusculas);
        System.out.println("Longitud: " + longitud);
        System.out.println("Primer caracter: " + caracter);
        System.out.println("Número entero formateado: " + numeroEnteroFormateado);
        System.out.println("Número decimal formateado: " + numeroDecimalFormateado);
    }
}
```

Explicación del código:

* Se declaran las variables necesarias para almacenar los datos del usuario y los resultados de las operaciones.
* Se solicita al usuario que ingrese un número entero, un número decimal y una cadena de texto.
* Se genera un número aleatorio.
* Se realizan las operaciones matemáticas básicas (suma, resta, multiplicación, división y módulo) entre los números ingresados por el usuario y el número aleatorio.
* Se realizan las operaciones con cadenas de texto (concatenación, conversión a mayúsculas y minúsculas, obtención de la longitud y obtención del primer carácter).
* Se formatea el número entero y el número decimal para mostrarlos con dos decimales.
* Se imprimen los resultados de las operaciones en la pantalla.