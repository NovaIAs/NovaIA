```java
public class CodigoComplejo {

    // Esta función recibe un número entero y devuelve su factorial.
    public static long factorial(int numero) {
        if (numero == 0) {
            return 1;
        } else {
            return numero * factorial(numero - 1);
        }
    }

    // Esta función recibe un array de números enteros y devuelve el máximo.
    public static int maximo(int[] array) {
        int maximo = array[0];
        for (int i = 1; i < array.length; i++) {
            if (array[i] > maximo) {
                maximo = array[i];
            }
        }
        return maximo;
    }

    // Esta función recibe una cadena de caracteres y devuelve su longitud.
    public static int longitud(String cadena) {
        int longitud = 0;
        for (char caracter : cadena.toCharArray()) {
            longitud++;
        }
        return longitud;
    }

    // Esta función recibe dos cadenas de caracteres y devuelve si son iguales.
    public static boolean sonIguales(String cadena1, String cadena2) {
        return cadena1.equals(cadena2);
    }

    // Esta función recibe dos números enteros y devuelve si el primero es divisible por el segundo.
    public static boolean esDivisible(int numero1, int numero2) {
        return numero1 % numero2 == 0;
    }

    // Esta función recibe un número entero y devuelve si es primo.
    public static boolean esPrimo(int numero) {
        if (numero <= 1) {
            return false;
        } else {
            for (int i = 2; i <= Math.sqrt(numero); i++) {
                if (numero % i == 0) {
                    return false;
                }
            }
            return true;
        }
    }

    // Esta función recibe un número entero y devuelve su signo.
    public static String signo(int numero) {
        if (numero > 0) {
            return "Positivo";
        } else if (numero < 0) {
            return "Negativo";
        } else {
            return "Cero";
        }
    }

    // Esta función recibe un número entero y devuelve su representación binaria.
    public static String binario(int numero) {
        StringBuilder binario = new StringBuilder();
        while (numero > 0) {
            binario.append(numero % 2);
            numero /= 2;
        }
        return binario.reverse().toString();
    }

    // Esta función recibe un número entero y devuelve su representación hexadecimal.
    public static String hexadecimal(int numero) {
        StringBuilder hexadecimal = new StringBuilder();
        char[] hexadecimales = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
        while (numero > 0) {
            hexadecimal.append(hexadecimales[numero % 16]);
            numero /= 16;
        }
        return hexadecimal.reverse().toString();
    }

    // Esta función recibe un número entero y devuelve su representación octal.
    public static String octal(int numero) {
        StringBuilder octal = new StringBuilder();
        while (numero > 0) {
            octal.append(numero % 8);
            numero /= 8;
        }
        return octal.reverse().toString();
    }

    // Esta función recibe un número entero y devuelve su representación en base 2 a la n.
    public static String baseN(int numero, int base) {
        StringBuilder baseN = new StringBuilder();
        while (numero > 0) {
            baseN.append(numero % base);
            numero /= base;
        }
        return baseN.reverse().toString();
    }

    // Esta función recibe dos números enteros y devuelve su máximo común divisor.
    public static int maximoComunDivisor(int numero1, int numero2) {
        while (numero2 != 0) {
            int temp = numero1;
            numero1 = numero2;
            numero2 = temp % numero2;
        }
        return numero1;
    }

    // Esta función recibe dos números enteros y devuelve su mínimo común múltiplo.
    public static int minimoComunMultiplo(int numero1, int numero2) {
        return (numero1 * numero2) / maximoComunDivisor(numero1, numero2);
    }

    // Esta función recibe un número entero y devuelve su raíz cuadrada.
    public static double raizCuadrada(int numero) {
        return Math.sqrt(numero);
    }

    // Esta función recibe un número entero y devuelve su raíz cúbica.
    public static double raizCubica(int numero) {
        return Math.cbrt(numero);
    }

    // Esta función recibe un número entero y devuelve su porcentaje.
    public static double porcentaje(int numero, int porcentaje) {
        return (numero * porcentaje) / 100;
    }

    // Esta función recibe un número entero y devuelve su seno.
    public static double seno(int numero) {
        return Math.sin(numero);
    }

    // Esta función recibe un número entero y devuelve su coseno.
    public static double coseno(int numero) {
        return Math.cos(numero);
    }

    // Esta función recibe un número entero y devuelve su tangente.
    public static double tangente(int numero) {
        return Math.tan(numero);
    }

    // Esta función recibe dos números enteros y devuelve su máximo común divisor.
    public static int maximoComunDivisor(int numero1, int numero2) {
        while (numero2 != 0) {
            int temp = numero1;
            numero1 = numero2;
            numero2 = temp % numero2;
        }
        return numero1;
    }

    // Esta función recibe dos números enteros y devuelve su mínimo común múltiplo.
    public static int minimoComunMultiplo(int numero1, int numero2) {
        return (numero1 * numero2) / maximoComunDivisor(numero1, numero2);
    }

    // Esta función recibe un número entero y devuelve su raíz cuadrada.
    public static double raizCuadrada(int numero) {
        return Math.sqrt(numero);
    }

    // Esta función recibe un número entero y devuelve su raíz cúbica.
    public static double raizCubica(int numero) {
        return Math.cbrt(numero);
    }

    // Esta función recibe un número entero y devuelve su porcentaje.
    public static double porcentaje(int numero, int porcentaje) {
        return (numero * porcentaje) / 100;
    }

    // Esta función recibe un número entero y devuelve su seno.
    public static double seno(int numero) {
        return Math.sin(numero);
    }

    // Esta función recibe un número entero y devuelve su coseno.
    public static double cos