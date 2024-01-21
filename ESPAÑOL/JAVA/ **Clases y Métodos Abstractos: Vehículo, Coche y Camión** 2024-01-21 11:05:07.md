```java
// Programa de ejemplo para demostrar una clase abstracta y sus implementaciones.
// Clase abstracta Vehículo.
abstract class Vehiculo {
    private String marca;
    private String modelo;
    private int año;

    // Constructor para la clase Vehículo.
    public Vehiculo(String marca, String modelo, int año) {
        this.marca = marca;
        this.modelo = modelo;
        this.año = año;
    }

    // Métodos abstractos para acelerar y frenar.
    public abstract void acelerar();
    public abstract void frenar();

    // Método para obtener la marca del vehículo.
    public String getMarca() {
        return marca;
    }

    // Método para obtener el modelo del vehículo.
    public String getModelo() {
        return modelo;
    }

    // Método para obtener el año del vehículo.
    public int getAño() {
        return año;
    }
}

// Clase Coche que implementa la clase abstracta Vehículo.
class Coche extends Vehiculo {
    private int numPuertas;

    // Constructor para la clase Coche.
    public Coche(String marca, String modelo, int año, int numPuertas) {
        super(marca, modelo, año);
        this.numPuertas = numPuertas;
    }

    // Implementación del método acelerar para la clase Coche.
    @Override
    public void acelerar() {
        System.out.println("El coche está acelerando.");
    }

    // Implementación del método frenar para la clase Coche.
    @Override
    public void frenar() {
        System.out.println("El coche está frenando.");
    }

    // Método para obtener el número de puertas del coche.
    public int getNumPuertas() {
        return numPuertas;
    }
}

// Clase Camión que implementa la clase abstracta Vehículo.
class Camión extends Vehiculo {
    private int capacidadCarga;

    // Constructor para la clase Camión.
    public Camión(String marca, String modelo, int año, int capacidadCarga) {
        super(marca, modelo, año);
        this.capacidadCarga = capacidadCarga;
    }

    // Implementación del método acelerar para la clase Camión.
    @Override
    public void acelerar() {
        System.out.println("El camión está acelerando.");
    }

    // Implementación del método frenar para la clase Camión.
    @Override
    public void frenar() {
        System.out.println("El camión está frenando.");
    }

    // Método para obtener la capacidad de carga del camión.
    public int getCapacidadCarga() {
        return capacidadCarga;
    }
}

// Clase Principal que contiene el método main.
public class Principal {
    public static void main(String[] args) {
        // Creación de objetos de las clases Coche y Camión.
        Coche coche1 = new Coche("Toyota", "Corolla", 2023, 4);
        Camión camión1 = new Camión("Mercedes-Benz", "Actros", 2021, 10000);

        // Impresión de la información de los vehículos.
        System.out.println("Coche:");
        System.out.println("Marca: " + coche1.getMarca());
        System.out.println("Modelo: " + coche1.getModelo());
        System.out.println("Año: " + coche1.getAño());
        System.out.println("Número de puertas: " + coche1.getNumPuertas());
        coche1.acelerar();
        coche1.frenar();

        System.out.println("\nCamión:");
        System.out.println("Marca: " + camión1.getMarca());
        System.out.println("Modelo: " + camión1.getModelo());
        System.out.println("Año: " + camión1.getAño());
        System.out.println("Capacidad de carga: " + camión1.getCapacidadCarga());
        camión1.acelerar();
        camión1.frenar();
    }
}
```

Explicación del código:

* La clase `Vehiculo` es una clase abstracta que define los métodos `acelerar()` y `frenar()`.
* Las clases `Coche` y `Camión` implementan la clase abstracta `Vehiculo` y proporcionan implementaciones específicas para los métodos `acelerar()` y `frenar()`.
* La clase `Principal` contiene el método `main()` que crea objetos de las clases `Coche` y `Camión`, imprime su información y llama a sus métodos `acelerar()` y `frenar()`.