```java
// Programa que simula una calculadora científica en Java.

import java.util.Scanner;

public class CalculadoraCientifica {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // Mostrar el menú de opciones al usuario.
        System.out.println("Calculadora científica");
        System.out.println("=====================");
        System.out.println("1. Suma");
        System.out.println("2. Resta");
        System.out.println("3. Multiplicación");
        System.out.println("4. División");
        System.out.println("5. Potencia");
        System.out.println("6. Raíz cuadrada");
        System.out.println("7. Seno");
        System.out.println("8. Coseno");
        System.out.println("9. Tangente");
        System.out.println("10. Arco seno");
        System.out.println("11. Arco coseno");
        System.out.println("12. Arco tangente");
        System.out.println("13. Logaritmo");
        System.out.println("14. Exponencial");
        System.out.println("15. Salir");

        // Leer la opción elegida por el usuario.
        int opcion = scanner.nextInt();

        // Ejecutar la operación correspondiente a la opción elegida.
        switch (opcion) {
            case 1:
                System.out.println("Introduzca el primer número:");
                double numero1 = scanner.nextDouble();
                System.out.println("Introduzca el segundo número:");
                double numero2 = scanner.nextDouble();
                System.out.println("La suma de los dos números es: " + (numero1 + numero2));
                break;
            case 2:
                System.out.println("Introduzca el primer número:");
                numero1 = scanner.nextDouble();
                System.out.println("Introduzca el segundo número:");
                numero2 = scanner.nextDouble();
                System.out.println("La resta de los dos números es: " + (numero1 - numero2));
                break;
            case 3:
                System.out.println("Introduzca el primer número:");
                numero1 = scanner.nextDouble();
                System.out.println("Introduzca el segundo número:");
                numero2 = scanner.nextDouble();
                System.out.println("La multiplicación de los dos números es: " + (numero1 * numero2));
                break;
            case 4:
                System.out.println("Introduzca el primer número:");
                numero1 = scanner.nextDouble();
                System.out.println("Introduzca el segundo número:");
                numero2 = scanner.nextDouble();
                if (numero2 == 0) {
                    System.out.println("No se puede dividir por cero.");
                } else {
                    System.out.println("La división de los dos números es: " + (numero1 / numero2));
                }
                break;
            case 5:
                System.out.println("Introduzca el número:");
                numero1 = scanner.nextDouble();
                System.out.println("Introduzca el exponente:");
                double exponente = scanner.nextDouble();
                System.out.println("La potencia del número es: " + Math.pow(numero1, exponente));
                break;
            case 6:
                System