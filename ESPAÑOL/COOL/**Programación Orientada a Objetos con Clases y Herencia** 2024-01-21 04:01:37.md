```cool
clase Compútador {
    atributo nombre : String;
    atributo velocidad : Integer;
    atributo memoria : Integer;

    método constructor(nombre : String, velocidad : Integer, memoria : Integer) {
        this.nombre := nombre;
        this.velocidad := velocidad;
        this.memoria := memoria;
    }

    método imprimir() {
        stdout.println("Nombre: " + this.nombre);
        stdout.println("Velocidad: " + this.velocidad + " GHz");
        stdout.println("Memoria: " + this.memoria + " GB");
    }
}

clase ComputadoraPortátil extends Compútador {
    atributo batería : Integer;

    método constructor(nombre : String, velocidad : Integer, memoria : Integer, batería : Integer) {
        super(nombre, velocidad, memoria);
        this.batería := batería;
    }

    método imprimir() {
        super.imprimir();
        stdout.println("Batería: " + this.batería + "%");
    }
}

clase ComputadoraEscritorio extends Compútador {
    atributo tarjetaGráfica : String;

    método constructor(nombre : String, velocidad : Integer, memoria : Integer, tarjetaGráfica : String) {
        super(nombre, velocidad, memoria);
        this.tarjetaGráfica := tarjetaGráfica;
    }

    método imprimir() {
        super.imprimir();
        stdout.println("Tarjeta Gráfica: " + this.tarjetaGráfica);
    }
}

clase ProgramaPrincipal {
    método main() {
        computadoraPortátil := ComputadoraPortátil.constructor("MacBook Pro", 2.4, 8, 80);
        computadoraEscritorio := ComputadoraEscritorio.constructor("Alienware Aurora R13", 3.6, 32, "NVIDIA GeForce RTX 3090");

        stdout.println("Computadora Portátil:");
        computadoraPortátil.imprimir();

        stdout.println("Computadora Escritorio:");
        computadoraEscritorio.imprimir();
    }
}
```

**Explicación del código:**

* La clase `Computador` define los atributos y métodos comunes a todas las computadoras, como el nombre, la velocidad y la memoria.
* La clase `ComputadoraPortátil` extiende la clase `Computador` y añade el atributo `batería`.
* La clase `ComputadoraEscritorio` extiende la clase `Computador` y añade el atributo `tarjetaGráfica`.
* La clase `ProgramaPrincipal` contiene el método `main()` que instancia objetos de las clases `ComputadoraPortátil` y `ComputadoraEscritorio` y los imprime por pantalla.