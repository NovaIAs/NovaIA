```d
import std.stdio, std.string, std.algorithm, std.range;
import std.algorithm.find, std.algorithm.sort;

class Objeto {
    Nombre nombre;
    Peso peso;
    Precio precio;

    Objeto(Nombre nombre, Peso peso, Precio precio) {
        this.nombre = nombre;
        this.peso = peso;
        this.precio = precio;
    }
}

class Mochila {
    Peso capacidad;
    lista![Objeto] objetos;

    Mochila(Peso capacidad) {
        this.capacidad = capacidad;
    }

    void agregarObjeto(Objeto objeto) {
        if (objeto.peso <= this.capacidad) {
            this.objetos ~= objeto;
            this.capacidad -= objeto.peso;
        }
    }

    Peso pesoTotal() {
        Peso pesoTotal = 0;
        foreach (o; this.objetos) {
            pesoTotal += o.peso;
        }
        return pesoTotal;
    }

    Precio precioTotal() {
        Precio precioTotal = 0;
        foreach (o; this.objetos) {
            precioTotal += o.precio;
        }
        return precioTotal;
    }

    void ordenarPorPeso() {
        sort!(this.objetos, [](a, b) { a.peso > b.peso; });
    }

    void ordenarPorPrecio() {
        sort!(this.objetos, [](a, b) { a.precio > b.peso; });
    }
}

Objeto crearObjeto(Nombre nombre, Peso peso, Precio precio) {
    return new Objeto(nombre, peso, precio);
}

void main() {
    Mochila mochila = new Mochila(10);
    lista![Objeto] objetos = [
        crearObjeto("Manzana", 1, 1),
        crearObjeto("Naranja", 2, 2),
        crearObjeto("Plátano", 3, 3),
        crearObjeto("Uva", 4, 4),
        crearObjeto("Pera", 5, 5),
        crearObjeto("Melón", 6, 6),
        crearObjeto("Sandía", 7, 7),
        crearObjeto("Piña", 8, 8),
        crearObjeto("Mango", 9, 9),
        crearObjeto("Guayaba", 10, 10)
    ];

    mochila.ordenarPorPeso();
    foreach (o; objetos) {
        mochila.agregarObjeto(o);
    }

    writeln("Objetos añadidos a la mochila:");
    foreach (o; mochila.objetos) {
        writeln("\tNombre: ", o.nombre, "\tPeso: ", o.peso, "\tPrecio: ", o.precio);
    }

    writeln("Peso total de la mochila:", mochila.pesoTotal());
    writeln("Precio total de la mochila:", mochila.precioTotal());
}
```

Este código simula el problema de la mochila, que consiste en seleccionar un conjunto de objetos de una lista para que su peso total no supere la capacidad de la mochila y su precio total sea máximo.

El código define dos clases: ```Objeto``` y ```Mochila```. La clase ```Objeto``` representa un objeto con nombre, peso y precio. La clase ```Mochila``` representa una mochila con capacidad máxima y una lista de objetos.

El código define también una función ```crearObjeto``` que crea un objeto a partir de su nombre, peso y precio.

La función ```main``` crea una mochila con capacidad de 10 unidades de peso y una lista de 10 objetos con diferentes pesos y precios.

La función ```mochila.ordenarPorPeso()``` ordena los objetos de la mochila por peso en orden descendente.

La función ```foreach (o; mochila.objetos)``` itera sobre los objetos de la mochila.

La función ```mochila.agregarObjeto(o)``` añade un objeto a la mochila si su peso no supera la capacidad de la mochila.

Las funciones ```mochila.pesoTotal()``` y ```mochila.precioTotal()``` calculan el peso total y el precio total de los objetos de la mochila.

El código imprime por pantalla los objetos añadidos a la mochila, el peso total de la mochila y el precio total de la mochila.