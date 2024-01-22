```d
import std.stdio, std.algorithm, std.regex;

void main() {
    // Definir una lista de nombres
    auto nombres = ["Juan", "María", "Pedro", "Ana", "Luis"];

    // Usar map para transformar cada nombre en su primera letra
    auto primerasLetras = nombres.map!((nombre) => nombre[0]);

    // Usar filter para obtener los nombres que empiezan por "M"
    auto nombresConM = nombres.filter!((nombre) => nombre[0] == 'M');

    // Usar sort para ordenar los nombres en orden alfabético
    nombres.sort!();

    // Usar find para encontrar la posición del nombre "Pedro" en la lista
    auto posicionPedro = nombres.find!("Pedro");

    // Usar join para unir los nombres en una cadena separada por comas
    auto cadenaNombres = nombres.join!(", ");

    // Usar split para dividir la cadena de nombres en una lista de nombres
    auto listaNombresSeparados = cadenaNombres.split!(", ");

    // Usar regex para buscar el patrón "a" en la lista de nombres
    auto nombresConA = listaNombresSeparados.filter!((nombre) => nombre ~= "a");

    // Imprimir los resultados
    writeln("Primeras letras:", primerasLetras);
    writeln("Nombres con M:", nombresConM);
    writeln("Nombres ordenados:", nombres);
    writeln("Posición de Pedro:", posicionPedro);
    writeln("Cadena de nombres:", cadenaNombres);
    writeln("Lista de nombres separados:", listaNombresSeparados);
    writeln("Nombres con A:", nombresConA);
}
```

Explicación:

* La función `main()` es el punto de entrada del programa.

* Las listas se definen con `[]`.

* La función `map!()` aplica una función a cada elemento de una lista y devuelve una nueva lista con los resultados.

* La función `filter!()` devuelve una lista con los elementos que cumplen una condición.

* La función `sort!()` ordena una lista en orden alfabético.

* La función `find!()` devuelve la posición del primer elemento en una lista que cumple una condición.

* La función `join!()` une los elementos de una lista en una cadena separada por un separador especificado.

* La función `split!()` divide una cadena en una lista de substrings.

* La función `regex` se utiliza para buscar patrones en una cadena.

* El operador `~=` se utiliza para comprobar si una cadena coincide con un patrón.

* La función `writeln()` imprime una línea de texto en la consola.