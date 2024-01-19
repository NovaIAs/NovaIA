```java
import java.util.*;
import java.lang.reflect.*;
import java.io.*;

public class Complejo {

    // Atributos
    private double real;
    private double imaginario;

    // Constructores
    public Complejo() {
        this.real = 0.0;
        this.imaginario = 0.0;
    }

    public Complejo(double real, double imaginario) {
        this.real = real;
        this.imaginario = imaginario;
    }

    // Métodos
    public String toString() {
        return String.format("(%.2f, %.2f)", real, imaginario);
    }

    public double getReal() {
        return real;
    }

    public double getImaginario() {
        return imaginario;
    }

    public void setReal(double real) {
        this.real = real;
    }

    public void setImaginario(double imaginario) {
        this.imaginario = imaginario;
    }

    public Complejo sumar(Complejo otro) {
        return new Complejo(real + otro.real, imaginario + otro.imaginario);
    }

    public Complejo restar(Complejo otro) {
        return new Complejo(real - otro.real, imaginario - otro.imaginario);
    }

    public Complejo multiplicar(Complejo otro) {
        return new Complejo(
            real * otro.real - imaginario * otro.imaginario,
            real * otro.imaginario + imaginario * otro.real
        );
    }

    public Complejo dividir(Complejo otro) {
        double denominador = otro.real * otro.real + otro.imaginario * otro.imaginario;
        return new Complejo(
            (real * otro.real + imaginario * otro.imaginario) / denominador,
            (imaginario * otro.real - real * otro.imaginario) / denominador
        );
    }

    // Métodos estáticos
    public static Complejo sumar(Complejo[] numeros) {
        Complejo resultado = new Complejo();
        for (Complejo numero : numeros) {
            resultado = resultado.sumar(numero);
        }
        return resultado;
    }

    public static Complejo restar(Complejo numero1, Complejo numero2) {
        return numero1.restar(numero2);
    }

    public static Complejo multiplicar(Complejo numero1, Complejo numero2) {
        return numero1.multiplicar(numero2);
    }

    public static Complejo dividir(Complejo numero1, Complejo numero2) {
        return numero1.dividir(numero2);
    }

    // Métodos de reflexión
    public static void main(String[] args) throws Exception {
        // Crear una lista de objetos Complejo
        List<Complejo> numeros = new ArrayList<>();
        numeros.add(new Complejo(1.0, 2.0));
        numeros.add(new Complejo(3.0, 4.0));
        numeros.add(new Complejo(5.0, 6.0));

        // Obtener la clase Complejo
        Class<?> claseComplejo = Complejo.class;

        // Obtener los métodos de la clase Complejo
        Method[] metodos = claseComplejo.getDeclaredMethods();

        // Iterar sobre los métodos de la clase Complejo
        for (Method metodo : metodos) {
            // Obtener el nombre del método
            String nombreMetodo = metodo.getName();

            // Comprobar si el método es público
            if (Modifier.isPublic(metodo.getModifiers())) {
                // Obtener los parámetros del método
                Class<?>[] parametros = metodo.getParameterTypes();

                // Comprobar si el método no tiene parámetros
                if (parametros.length == 0) {
                    // Invocar el método sin parámetros
                    Object resultado = metodo.invoke(numeros.get(0));

                    // Obtener el tipo de retorno del método
                    Class<?> tipoRetorno = metodo.getReturnType();

                    // Comprobar si el tipo de retorno es un objeto Complejo
                    if (tipoRetorno == Complejo.class) {
                        // Convertir el objeto a un objeto Complejo
                        Complejo resultadoComplejo = (Complejo) resultado;

                        // Imprimir el resultado del método
                        System.out.println(nombreMetodo + ": " + resultadoComplejo);
                    } else {
                        // Imprimir el resultado del método
                        System.out.println(nombreMetodo + ": " + resultado);
                    }
                } else {
                    // Obtener el primer parámetro del método
                    Class<?> primerParametro = parametros[0];

                    // Comprobar si el primer parámetro es un objeto Complejo
                    if (primerParametro == Complejo.class) {
                        // Invocar el método con el primer parámetro como argumento
                        Object resultado = metodo.invoke(numeros.get(0), numeros.get(1));

                        // Obtener el tipo de retorno del método
                        Class<?> tipoRetorno = metodo.getReturnType();

                        // Comprobar si el tipo de retorno es un objeto Complejo
                        if (tipoRetorno == Complejo.class) {
                            // Convertir el objeto a un objeto Complejo
                            Complejo resultadoComplejo = (Complejo) resultado;

                            // Imprimir el resultado del método
                            System.out.println(nombreMetodo + ": " + resultadoComplejo);
                        } else {
                            // Imprimir el resultado del método
                            System.out.println(nombreMetodo + ": " + resultado);
                        }
                    } else {
                        // Invocar el método con el primer parámetro como argumento
                        Object resultado = metodo.invoke(numeros.get(0), 1.0);

                        // Obtener el tipo de retorno del método
                        Class<?> tipoRetorno = metodo.getReturnType();

                        // Comprobar si el tipo de retorno es un objeto Complejo
                        if (tipoRetorno == Complejo.class) {
                            // Convertir el objeto a un objeto Complejo
                            Complejo resultadoComplejo = (Complejo) resultado;

                            // Imprimir el resultado del método
                            System.out.println(nombreMetodo + ": " + resultadoComplejo);
                        } else {
                            // Imprimir el resultado del método
                            System.out.println(nombreMetodo + ": " + resultado);
                        }
                    }
                }
            }
        }

        // Imprimir la suma de los números
        Complejo suma = Complejo.sumar(numeros.toArray(new Complejo[0]));
        System.out.println("Suma: " + suma);

        // Imprimir la resta de los números
        Complejo resta = Complejo.restar(numeros.get(0), numeros.get(1));
        System.out.println("Resta: " + resta);

        // Imprimir la multiplicación de los números
        Complejo multiplicacion = Complejo.multiplicar(numeros.get(0), numeros.get(1));
        System.ou