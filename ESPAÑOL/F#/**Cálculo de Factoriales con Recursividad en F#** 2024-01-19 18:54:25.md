```f#
// Definición de una función recursiva para calcular el factorial de un número
let factorial n =
    if n == 0 then
        1 // Caso base: el factorial de 0 es 1
    else
        n * factorial (n - 1) // Caso recursivo: el factorial de n es n * factorial(n-1)

// Definición de una lista de números
let numbers = [1; 2; 3; 4; 5]

// Utilizar la función map para aplicar la función factorial a cada elemento de la lista
let factorials = numbers |> List.map factorial

// Imprimir los resultados en la consola
for n in factorials do
    printfn "%d -> %d" n n

```

Explicación del código:

1. **Función factorial**: Definimos una función recursiva llamada `factorial` que calcula el factorial de un número. El factorial de un número es el producto de todos los números enteros positivos menores o iguales a ese número.

2. **Caso base**: La función tiene un caso base para cuando el número es 0. En este caso, la función devuelve 1, ya que el factorial de 0 es 1.

3. **Caso recursivo**: Para todos los números mayores que 0, la función multiplica el número por el factorial del número menos 1. Esto se hace mediante una llamada recursiva a la función `factorial`.

4. **Lista de números**: Definimos una lista de números llamada `numbers` que contiene los números del 1 al 5.

5. **Función map**: Utilizamos la función `map` para aplicar la función `factorial` a cada elemento de la lista `numbers`. La función `map` toma una función y una lista como argumentos, y devuelve una nueva lista con los resultados de aplicar la función a cada elemento de la lista.

6. **Imprimir los resultados**: Por último, utilizamos un bucle `for` para imprimir los resultados de la función `factorial` en la consola.

El resultado del código será:

```
1 -> 1
2 -> 2
3 -> 6
4 -> 24
5 -> 120
```