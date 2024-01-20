```d
import std.stdio, std.range;

// Define un enumerado para los días de la semana
enum DiaDeLaSemana : byte {
    Lunes,
    Martes,
    Miércoles,
    Jueves,
    Viernes,
    Sábado,
    Domingo
}

// Define una estructura para representar una fecha
struct Fecha {
    int año;
    int mes;
    int día;
}

// Define una función para obtener el día de la semana de una fecha
DiaDeLaSemana obtenerDiaDeLaSemana(Fecha fecha) {
    // Utiliza el algoritmo de Zeller para calcular el día de la semana
    int a = (fecha.año - 1) % 100;
    int b = (fecha.año - 1) / 100;
    int c = fecha.día;
    int d = fecha.mes;
    int e = (int)(13 * (d + 1) / 5);
    int f = a + b + c + d + e - 2;
    int g = (int)(f % 7);
    if (g < 0) g += 7;
    return (DiaDeLaSemana)g;
}

// Define una función para imprimir una fecha en formato español
void imprimirFechaEnEspañol(Fecha fecha) {
    // Obtiene el nombre del día de la semana
    DiaDeLaSemana díaDeLaSemana = obtenerDiaDeLaSemana(fecha);
    String nombreDelDíaDeLaSemana = díaDeLaSemana.to!string;

    // Imprime la fecha en formato español
    writefln("Fecha: %s, %d de %s de %d",
             nombreDelDíaDeLaSemana,
             fecha.día,
             fecha.mes.to!string,
             fecha.año);
}

void main() {
    // Define una fecha
    Fecha fecha = {
        .año = 2023,
        .mes = 8,
        .día = 15
    };

    // Imprime la fecha en formato español
    imprimirFechaEnEspañol(fecha);
}
```

Explicación del código:

* El código anterior define un enumerado llamado `DiaDeLaSemana` para representar los días de la semana.
* También define una estructura llamada `Fecha` para representar una fecha.
* La función `obtenerDiaDeLaSemana` utiliza el algoritmo de Zeller para calcular el día de la semana de una fecha.
* La función `imprimirFechaEnEspañol` imprime una fecha en formato español.
* La función `main` define una fecha y la imprime en formato español utilizando la función `imprimirFechaEnEspañol`.

El código anterior es un ejemplo de código complejo en D. Es un ejemplo de cómo se pueden utilizar los enumerados, las estructuras y las funciones para crear programas complejos.