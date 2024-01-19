```groovy
//Clase Persona
class Persona {
    String nombre
    int edad
    String cedula

    Persona(String nombre, int edad, String cedula) {
        this.nombre = nombre
        this.edad = edad
        this.cedula = cedula
    }

    //Método para obtener el nombre de la persona
    String obtenerNombre() {
        return nombre
    }

    //Método para obtener la edad de la persona
    int obtenerEdad() {
        return edad
    }

    //Método para obtener la cédula de la persona
    String obtenerCedula() {
        return cedula
    }
}

//Clase Calculadora
class Calculadora {
    //Método para sumar dos números
    int sumar(int a, int b) {
        return a + b
    }

    //Método para restar dos números
    int restar(int a, int b) {
        return a - b
    }

    //Método para multiplicar dos números
    int multiplicar(int a, int b) {
        return a * b
    }

    //Método para dividir dos números
    int dividir(int a, int b) {
        return a / b
    }
}

//Clase Main
class Main {
    //Método principal
    static void main(String[] args) {
        //Creamos un objeto de la clase Persona
        Persona persona = new Persona("Juan", 25, "123456789")

        //Imprimimos el nombre de la persona
        println("El nombre de la persona es: ${persona.obtenerNombre()}")

        //Imprimimos la edad de la persona
        println("La edad de la persona es: ${persona.obtenerEdad()}")

        //Imprimimos la cédula de la persona
        println("La cédula de la persona es: ${persona.obtenerCedula()}")

        //Creamos un objeto de la clase Calculadora
        Calculadora calculadora = new Calculadora()

        //Imprimimos el resultado de sumar 10 y 20
        println("El resultado de sumar 10 y 20 es: ${calculadora.sumar(10, 20)}")

        //Imprimimos el resultado de restar 10 y 20
        println("El resultado de restar 10 y 20 es: ${calculadora.restar(10, 20)}")

        //Imprimimos el resultado de multiplicar 10 y 20
        println("El resultado de multiplicar 10 y 20 es: ${calculadora.multiplicar(10, 20)}")

        //Imprimimos el resultado de dividir 10 y 20
        println("El resultado de dividir 10 y 20 es: ${calculadora.dividir(10, 20)}")
    }
}
```

Este código crea una clase Persona con tres propiedades: nombre, edad y cédula. También crea una clase Calculadora con cuatro métodos: sumar, restar, multiplicar y dividir. Por último, crea una clase Main que crea objetos de las clases Persona y Calculadora y llama a sus métodos para imprimir el nombre, la edad, la cédula y los resultados de las operaciones aritméticas en la consola.