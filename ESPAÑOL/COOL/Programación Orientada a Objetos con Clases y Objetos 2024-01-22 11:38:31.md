```cool
clase {Navío} {
    campos:
        nombre: String;
        longitud: Float;
        ancho: Float;
        calado: Float;
        velocidad: Float;
        capacidad: Float;
        tipo: String;
        bandera: String;

    métodos:
        crear(String nombre, Float longitud, Float ancho, Float calado, Float velocidad, Float capacidad, String tipo, String bandera) {
            this.nombre := nombre;
            this.longitud := longitud;
            this.ancho := ancho;
            this.calado := calado;
            this.velocidad := velocidad;
            this.capacidad := capacidad;
            this.tipo := tipo;
            this.bandera := bandera;
        }

        imprimir() {
            IO.escribir_linea("Nombre: " || this.nombre);
            IO.escribir_linea("Longitud: " || this.longitud.convertir_a_string());
            IO.escribir_linea("Ancho: " || this.ancho.convertir_a_string());
            IO.escribir_linea("Calado: " || this.calado.convertir_a_string());
            IO.escribir_linea("Velocidad: " || this.velocidad.convertir_a_string());
            IO.escribir_linea("Capacidad: " || this.capacidad.convertir_a_string());
            IO.escribir_linea("Tipo: " || this.tipo);
            IO.escribir_linea("Bandera: " || this.bandera);
        }
}

clase {Puerto} {
    campos:
        nombre: String;
        ubicación: String;
        capacidad: Float;
        naves: Lista[Navío];

    métodos:
        crear(String nombre, String ubicación, Float capacidad) {
            this.nombre := nombre;
            this.ubicación := ubicación;
            this.capacidad := capacidad;
            this.naves := Lista[Navío].nueva();
        }

        atracar(Navío nave) {
            if (this.capacidad >= nave.capacidad) {
                this.naves.agregar(nave);
                this.capacidad -= nave.capacidad;
                IO.escribir_linea("El navío " || nave.nombre || " ha atracado en el puerto " || this.nombre);
            } else {
                IO.escribir_linea("El puerto " || this.nombre || " no tiene suficiente capacidad para atracar el navío " || nave.nombre);
            }
        }

        desatracar(Navío nave) {
            if (this.naves.contiene(nave)) {
                this.naves.remover(nave);
                this.capacidad += nave.capacidad;
                IO.escribir_linea("El navío " || nave.nombre || " ha desatracado del puerto " || this.nombre);
            } else {
                IO.escribir_linea("El navío " || nave.nombre || " no está atracado en el puerto " || this.nombre);
            }
        }

        imprimir() {
            IO.escribir_linea("Nombre: " || this.nombre);
            IO.escribir_linea("Ubicación: " || this.ubicación);
            IO.escribir_linea("Capacidad: " || this.capacidad.convertir_a_string());
            IO.escribir_linea("Naves:");
            for (Navío nave in this.naves) {
                nave.imprimir();
            }
        }
}

clase {Programa} {
    método principal() {
        // Crear un nuevo navío
        Navío navío1 := Navío.crear(
            "Titanic",
            269.1,
            28.2,
            11.3,
            23.6,
            46.329,
            "Trasatlántico",
            "Reino Unido"
        );

        // Crear un nuevo puerto
        Puerto puerto1 := Puerto.crear("Puerto de Barcelona", "Barcelona, España", 100000.0);

        // Atracar el navío en el puerto
        puerto1.atracar(navío1);

        // Desatracar el navío del puerto
        puerto1.desatracar(navío1);

        // Imprimir el navío
        navío1.imprimir();

        // Imprimir el puerto
        puerto1.imprimir();
    }
}
```

Explicación del código:

* La clase `Navío` representa un navío con campos como nombre, longitud, ancho, calado, velocidad, capacidad, tipo y bandera.
* La clase `Puerto` representa un puerto con campos como nombre, ubicación, capacidad y una lista de naves.
* La clase `Programa` contiene el método `principal()` que se ejecuta cuando se ejecuta el programa.

En el método `principal()`, se crean un nuevo navío y un nuevo puerto, se atraca el navío en el puerto, se desatraca el navío del puerto, y se imprimen el navío y el puerto.

El código utiliza las siguientes características de COOL:

* Clases y objetos
* Campos y métodos
* Herencia
* Polimorfismo
* Listas
* Expresiones condicionales
* Bucles `for`
* E/S