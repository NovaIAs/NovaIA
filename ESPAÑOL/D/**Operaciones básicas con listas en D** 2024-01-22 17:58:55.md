```d
// Import the necessary libraries.
import std.stdio;
import std.algorithm;

// Define the main function.
void main() {

    // Create a list of numbers.
    auto numbers = [1, 2, 3, 4, 5];

    // Print the original list.
    writeln("Original list:");
    for (auto number : numbers) {
        writef("%d ", number);
    }
    writeln;

    // Sort the list in ascending order.
    numbers.sort();

    // Print the sorted list.
    writeln("Sorted list:");
    for (auto number : numbers) {
        writef("%d ", number);
    }
    writeln;

    // Find the maximum value in the list.
    auto max_value = numbers.max;

    // Find the minimum value in the list.
    auto min_value = numbers.min;

    // Calculate the sum of all the values in the list.
    auto sum = numbers.sum;

    // Calculate the average value of all the values in the list.
    auto average = sum / numbers.length;

    // Print the maximum, minimum, sum, and average values.
    writeln("Maximum value:", max_value);
    writeln("Minimum value:", min_value);
    writeln("Sum of all values:", sum);
    writeln("Average value:", average);

    // Check if the list contains a certain value.
    auto value_to_find = 3;
    auto found = numbers.find(value_to_find);

    // Print whether the value was found or not.
    if (found != null) {
        writeln("Value", value_to_find, "found at index", found);
    } else {
        writeln("Value", value_to_find, "not found");
    }

    // Remove the first occurrence of a certain value from the list.
    auto value_to_remove = 2;
    numbers.remove(value_to_remove);

    // Print the list after removing the value.
    writeln("List after removing value", value_to_remove, ":");
    for (auto number : numbers) {
        writef("%d ", number);
    }
    writeln;

    // Insert a new value at a specific index in the list.
    auto value_to_insert = 6;
    auto index_to_insert_at = 2;
    numbers.insert(index_to_insert_at, value_to_insert);

    // Print the list after inserting the value.
    writeln("List after inserting value", value_to_insert, "at index", index_to_insert_at, ":");
    for (auto number : numbers) {
        writef("%d ", number);
    }
    writeln;

}
```

Explicación del código:

* El código comienza importando las bibliotecas necesarias.
* Se define la función `main` que es la función principal del programa.
* Se crea una lista de números.
* Se imprime la lista original.
* Se ordena la lista en orden ascendente.
* Se imprime la lista ordenada.
* Se encuentra el valor máximo en la lista.
* Se encuentra el valor mínimo en la lista.
* Se calcula la suma de todos los valores de la lista.
* Se calcula la media de todos los valores de la lista.
* Se imprime el valor máximo, el valor mínimo, la suma y la media.
* Se comprueba si la lista contiene un valor determinado.
* Se imprime si el valor se ha encontrado o no.
* Se elimina la primera aparición de un valor determinado de la lista.
* Se imprime la lista después de eliminar el valor.
* Se inserta un nuevo valor en un índice determinado de la lista.
* Se imprime la lista después de insertar el valor.