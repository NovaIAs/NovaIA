```d
import std.stdio, std.algorithm, std.typecons, std.range;

alias Móvil = struct {
    string nombre, tipo
    int puertas
    float precio
};

alias Moto = struct {
    string nombre, tipo
    int cilindrada
    float precio
};

interface Vehículo {
    void manejar() pure nothrow;
};

class Coche : Móvil, Vehículo {

    override void manejar() {
        writefln("Estoy manejando un coche llamado %s", nombre);
    }

};

class Motocicleta : Moto, Vehículo {

    override void manejar() {
        writefln("Estoy manejando una moto llamada %s", nombre);
    }

};

void main() {

    auto coches = new Móvil[] {
        new Coche{nombre = "BMW", tipo = "Sedan", puertas = 4, precio = 30000},
        new Coche{nombre = "Tesla", tipo = "Eléctrico", puertas = 4, precio = 40000},
        new Coche{nombre = "Ford", tipo = "Camioneta", puertas = 2, precio = 20000}
    };

    auto motos = new Moto[] {
        new Motocicleta{nombre = "Honda", tipo = "Deportiva", cilindrada = 1000, precio = 10000},
        new Motocicleta{nombre = "Harley", tipo = "Crucero", cilindrada = 1200, precio = 15000},
        new Motocicleta{nombre = "Suzuki", tipo = "Naked", cilindrada = 600, precio = 8000}
    };

    foreach (coche; coches) {
        coche.manejar();
    }

    foreach (moto; motos) {
        moto.manejar();
    }

    // Ordenar los coches por precio
    coches.sort!{|a, b| a.precio <=> b.precio};

    writefln("Coches ordenados por precio:");
    foreach (coche; coches) {
        writefln("Nombre: %s, Precio: %f", coche.nombre, coche.precio);
    }

    // Obtener el coche más caro y la moto más barata
    auto cocheMasCaro = coches.max!{|a, b| a.precio <=> b.precio};
    auto motoMasBarata = motos.min!{|a, b| a.precio <=> b.precio};

    writefln("El coche más caro es: %s, con un precio de %f", cocheMasCaro.nombre, cocheMasCaro.precio);
    writefln("La moto más barata es: %s, con un precio de %f", motoMasBarata.nombre, motoMasBarata.precio);

    // Filtrar los coches con un precio mayor a 30000
    auto cochesCaros = coches.filter!{|coche| coche.precio > 30000};

    writefln("Coches con un precio mayor a 30000:");
    foreach (coche; cochesCaros) {
        writefln("Nombre: %s, Precio: %f", coche.nombre, coche.precio);
    }

}
```

Explicación del código:

1. **Definición de las estructuras `Móvil` y `Moto`:**
    * `Móvil` es una estructura que representa un vehículo con características generales como el nombre, el tipo, el número de puertas y el precio.
    * `Moto` es una estructura que representa una motocicleta con características específicas como la cilindrada y el tipo.

2. **Definición de la interfaz `Vehículo`:**
    * La interfaz `Vehículo` define un método `manejar()` que debe ser implementado por todas las clases que representen vehículos.

3. **Definición de las clases `Coche` y `Motocicleta`:**
    * `Coche` es una clase que hereda de `Móvil` e implementa la interfaz `Vehículo`.
    * `Motocicleta` es una clase que hereda de `Moto` e implementa la interfaz `Vehículo`.
    * Ambos clases anulan el método `manejar()` para imprimir un mensaje específico para cada tipo de vehículo.

4. **Función `main()`:**
    * La función `main()` es el punto de entrada del programa.

5. **Creación de arreglos de coches y motos:**
    * Se crean dos arreglos, uno para los coches y otro para las motos, utilizando la sintaxis de arreglos de D.

6. **Iteración sobre los arreglos de coches y motos:**
    * Se utiliza la sintaxis de iteración `foreach` para iterar sobre los arreglos de coches y motos e invocar el método `manejar()` para cada vehículo.

7. **Ordenar los coches por precio:**
    * Se utiliza el método `sort!()` para ordenar los coches por precio en orden ascendente.

8. **Obtener el coche más caro y la moto más barata:**
    * Se utilizan los métodos `max!()` y `min!()` para obtener el coche más caro y la moto más barata, respectivamente.

9. **Filtrar los coches con un precio mayor a 30000:**
    * Se utiliza el método `filter!()` para filtrar los coches con un precio mayor a 30000.