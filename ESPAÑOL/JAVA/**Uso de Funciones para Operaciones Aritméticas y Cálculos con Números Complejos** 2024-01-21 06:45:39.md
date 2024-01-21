```java

import java.util.Scanner;

public class Complejo {

    public static void main(String[] args) {

        // Declaración de variables
        double parteReal;
        double parteImaginaria;
        Complejo complejo1;
        Complejo complejo2;
        Complejo resultado;
        Scanner entrada = new Scanner(System.in);

        // Pedir al usuario que introduzca los valores de los dos números complejos
        System.out.print("Introduzca la parte real del primer número complejo: ");
        parteReal = entrada.nextDouble();
        System.out.print("Introduzca la parte imaginaria del primer número complejo: ");
        parteImaginaria = entrada.nextDouble();
        complejo1 = new Complejo(parteReal, parteImaginaria);

        System.out.print("Introduzca la parte real del segundo número complejo: ");
        parteReal = entrada.nextDouble();
        System.out.print("Introduzca la parte imaginaria del segundo número complejo: ");
        parteImaginaria = entrada.nextDouble();
        complejo2 = new Complejo(parteReal, parteImaginaria);

        // Realizar las operaciones aritméticas entre los dos números complejos
        resultado = complejo1.suma(complejo2);
        System.out.println("La suma de los dos números complejos es: " + resultado.toString());

        resultado = complejo1.resta(complejo2);
        System.out.println("La resta de los dos números complejos es: " + resultado.toString());

        resultado = complejo1.multiplicacion(complejo2);
        System.out.println("La multiplicación de los dos números complejos es: " + resultado.toString());

        resultado = complejo1.division(complejo2);
        System.out.println("La división de los dos números complejos es: " + resultado.toString());

        // Pedir al usuario que introduzca un número real para multiplicar el número complejo
        System.out.print("Introduzca un número real para multiplicar el número complejo: ");
        double numeroReal = entrada.nextDouble();

        // Realizar la multiplicación del número complejo por el número real
        resultado = complejo1.multiplicacion(numeroReal);
        System.out.println("El número complejo multiplicado por el número real es: " + resultado.toString());

        // Pedir al usuario que introduzca un ángulo en grados para calcular el módulo del número complejo
        System.out.print("Introduzca un ángulo en grados para calcular el módulo del número complejo: ");
        double anguloGrados = entrada.nextDouble();

        // Convertir el ángulo de grados a radianes
        double anguloRadianes = Math.toRadians(anguloGrados);

        // Calcular el módulo del número complejo
        double modulo = complejo1.modulo(anguloRadianes);
        System.out.println("El módulo del número complejo es: " + modulo);

        // Pedir al usuario que introduzca un ángulo en grados para calcular el argumento del número complejo
        System.out.print