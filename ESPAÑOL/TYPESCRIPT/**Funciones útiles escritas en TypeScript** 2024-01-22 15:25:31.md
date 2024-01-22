```typescript
// DEFINICIÓN DE FUNCIONES

// Función para saludar a alguien
function saludar(nombre: string): string {
    return `Hola, ${nombre}!`;
}

// Función para calcular el área de un rectángulo
function areaRectangulo(base: number, altura: number): number {
    return base * altura;
}

// Función para calcular el factorial de un número
function factorial(numero: number): number {
    if (numero <= 1) {
        return 1;
    } else {
        return numero * factorial(numero - 1);
    }
}

// Función para ordenar un array de números
function ordenarArray(array: number[]): number[] {
    return array.sort((a, b) => a - b);
}

// Función para buscar un elemento en un array
function buscarElemento(array: number[], elemento: number): number {
    return array.indexOf(elemento);
}

// FUNCIÓN PRINCIPAL

// Programa principal
function main(): void {
    // Saludo al usuario
    console.log(saludar("Juan"));

    // Cálculo del área de un rectángulo
    console.log(`El área del rectángulo es ${areaRectangulo(5, 10)}`);

    // Cálculo del factorial de un número
    console.log(`El factorial de 5 es ${factorial(5)}`);

    // Ordenación de un array
    console.log(`El array ordenado es ${ordenarArray([1, 5, 2, 3, 4])}`);

    // Búsqueda de un elemento en un array
    console.log(`El índice del elemento 3 en el array es ${buscarElemento([1, 5, 2, 3, 4], 3)}`);
}

// LLAMADA A LA FUNCIÓN PRINCIPAL

// Llamada a la función principal
main();
```

**Explicación del código:**

El código anterior es un programa escrito en TypeScript que realiza varias tareas comunes, como saludar a alguien, calcular el área de un rectángulo, calcular el factorial de un número, ordenar un array de números y buscar un elemento en un array.

El programa consta de varias funciones, cada una de las cuales realiza una tarea específica. Las funciones están definidas en la parte superior del programa.

La función `saludar` toma un nombre como argumento y devuelve un saludo. La función `areaRectangulo` toma dos argumentos, la base y la altura de un rectángulo, y devuelve el área del rectángulo. La función `factorial` toma un número como argumento y devuelve el factorial del número. La función `ordenarArray` toma un array de números como argumento y devuelve el array ordenado. La función `buscarElemento` toma un array de números y un elemento como argumentos, y devuelve el índice del elemento en el array.

La función `main` es la función principal del programa. Esta función llama a las demás funciones para realizar las tareas necesarias.

El programa comienza llamando a la función `main`. La función `main` saluda al usuario, calcula el área de un rectángulo, calcula el factorial de un número, ordena un array de números y busca un elemento en un array.

El resultado del programa se imprime en la consola.