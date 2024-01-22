```java
// Clase principal del programa
public class ProgramaComplejo {

    // Método principal del programa
    public static void main(String[] args) {

        // Definir variables
        int[] numeros = {1, 2, 3, 4, 5};
        String[] nombres = {"Juan", "Pedro", "María", "Ana", "Luis"};
        List<Integer> listaNumeros = new ArrayList<>();
        Map<String, Integer> mapaNombres = new HashMap<>();

        // Bucle for-each para recorrer el array de números
        for (int numero : numeros) {
            System.out.println("Número: " + numero);
        }

        // Bucle for-each para recorrer el array de nombres
        for (String nombre : nombres) {
            System.out.println("Nombre: " + nombre);
        }

        // Bucle for para recorrer la lista de números
        for (int i = 0; i < listaNumeros.size(); i++) {
            System.out.println("Número: " + listaNumeros.get(i));
        }

        // Bucle for para recorrer el mapa de nombres
        for (Map.Entry<String, Integer> entry : mapaNombres.entrySet()) {
            System.out.println("Nombre: " + entry.getKey() + ", Edad: " + entry.getValue());
        }

        // Bucle while para contar hasta 10
        int contador = 0;
        while (contador < 10) {
            System.out.println("Contador: " + contador);
            contador++;
        }

        // Bucle do-while para contar hasta 10
        contador = 0;
        do {
            System.out.println("Contador: " + contador);
            contador++;
        } while (contador < 10);

        // Sentencia condicional if-else para comprobar si un número es par o impar
        int numero = 5;
        if (numero % 2 == 0) {
            System.out.println("El número " + numero + " es par.");
        } else {
            System.out.println("El número " + numero + " es impar.");
        }

        // Sentencia condicional switch-case para comprobar el día de la semana
        int dia = 3;
        switch (dia) {
            case 1:
                System.out.println("Lunes");
                break;
            case 2:
                System.out.println("Martes");
                break;
            case 3:
                System.out.println("Miércoles");
                break;
            case 4:
                System.out.println("Jueves");
                break;
            case 5:
                System.out.println("Viernes");
                break;
            case 6:
                System.out.println("Sábado");
                break;
            case 7:
                System.out.println("Domingo");
                break;
            default:
                System.out.println("Día no válido.");
        }

        // Sentencia try-catch para capturar excepciones
        try {
            // Código que puede generar una excepción
            int resultado = 10 / 0;
            System.out.println("Resultado: " + resultado);
        } catch (ArithmeticException e) {
            // Código que se ejecuta si se produce una excepción
            System.out.println("Se ha producido una excepción: " + e.getMessage());
        } finally {
            // Código que se ejecuta siempre, independientemente de si se produce o no una excepción
            System.out.println("Finalmente");
        }

        // Crear un objeto de la clase Persona
        Persona persona = new Persona("Juan", 25);

        // Acceder a los atributos del objeto persona
        System.out.println("Nombre: " + persona.getNombre());
        System.out.println("Edad: " + persona.getEdad());

        // Llamar al método saludar() del objeto persona
        persona.saludar();

        // Crear una lista de objetos de la clase Persona
        List<Persona> personas = new ArrayList<>();

        // Añadir objetos a la lista de personas
        personas.add(new Persona("Pedro", 30));
        personas.add(new Persona("María", 28));
        personas.add(new Persona("Ana", 22));

        // Recorrer la lista de personas e imprimir los datos de cada una
        for (Persona p : personas) {
            System.out.println("Nombre: " + p.getNombre() + ", Edad: " + p.getEdad());
        }

        // Crear un mapa de objetos de la clase Persona, donde la clave es el nombre y el valor es la edad
        Map<String, Integer> mapaPersonas = new HashMap<>();

        // Añadir objetos al mapa de personas
        mapaPersonas.put("Juan", 25);
        mapaPersonas.put("Pedro", 30);
        mapaPersonas.put("María", 28);
        mapaPersonas.put("Ana", 22);

        // Recorrer el mapa de personas e imprimir los datos de cada una
        for (Map.Entry<String, Integer> entry : mapaPersonas.entrySet()) {
            System.out.println("Nombre: " + entry.getKey() + ", Edad: " + entry.getValue());
        }

        // Crear una clase anónima que implemente la interfaz Runnable
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                System.out.println("Hilo anónimo");
            }
        };

        // Crear un nuevo hilo y ejecutarlo
        Thread hilo = new Thread(runnable);
        hilo.start();

        // Crear una lambda expression que implemente la interfaz Runnable
        Runnable lambda = () -> System.out.println("Hilo lambda");

        // Crear un nuevo hilo y ejecutarlo
        Thread hilo2 = new Thread(lambda);
        hilo2.start();
    }

    // Clase Persona
    private static class Persona {

        private String nombre;
        private int edad;

        public Persona(String nombre, int edad) {
            this.nombre = nombre;
            this.edad = edad;
        }

        public String getNombre() {
            return nombre;
        }

        public int getEdad() {
            return edad;
        }

        public void saludar() {
            System.out.println("Hola, mi nombre es " + nombre + " y tengo " + edad + " años.");
        }
    }
}
```

Explicación del código:

* El código crea un array de números, un array de nombres, una lista de números, un mapa de nombres, y un objeto de la clase Persona.
* El código recorre los arrays y las listas usando bucles for-each, for y while.
* El código utiliza sentencias condicionales if-else y switch-case para comprobar si un número es par o impar y para comprobar el día de la semana.
* El código utiliza una sentencia try-catch para capturar excepciones.
* El código crea una lista y un mapa de objetos de la clase Persona, y recorre la lista y el mapa usando bucles for-each y for.
* El código crea un hilo anónimo y un hilo usando una lambda expression, y ejecuta ambos hilos.

El código es muy complejo y difícil de repetir, ya que utiliza una variedad de características del lenguaje Java, incluyendo arrays, listas, mapas, objetos, sentencias condicionales, sentencias try-catch, hilos y lambda expressions.