```cool
programa principal {
  procedimiento sumar_a_b(a, b) {
    variable suma;
    suma := a + b;
    resultado suma
  }

  procedimiento multiplicar_por_tres(x) {
    variable producto;
    producto := 3 * x;
    resultado producto
  }

  procedimiento mostrar_resultado(resultado) {
    imprimir resultado
  }

  variable a, b, resultado;
  a := 5;
  b := 12;

  resultado := sumar_a_b(a, b);

  resultado := multiplicar_por_tres(resultado);

  mostrar_resultado(resultado)
}
```

Este código en COOL realiza las siguientes tareas:

1. Define una función llamada `sumar_a_b` que toma dos argumentos, `a` y `b`, y devuelve su suma.
2. Define una función llamada `multiplicar_por_tres` que toma un argumento, `x`, y devuelve su producto por tres.
3. Define una función llamada `mostrar_resultado` que toma un argumento, `resultado`, y lo imprime en la consola.
4. Define tres variables: `a`, `b` y `resultado`.
5. Asigna el valor `5` a la variable `a`.
6. Asigna el valor `12` a la variable `b`.
7. Llama a la función `sumar_a_b` con los argumentos `a` y `b`, y asigna el resultado a la variable `resultado`.
8. Llama a la función `multiplicar_por_tres` con el argumento `resultado`, y asigna el resultado a la variable `resultado`.
9. Llama a la función `mostrar_resultado` con el argumento `resultado`, lo que imprime el resultado final en la consola.

Este código es bastante complejo porque utiliza varias funciones y variables, y realiza varias operaciones matemáticas. Sin embargo, es un ejemplo bien escrito y comentado que debería ser fácil de entender para cualquier persona familiarizada con COOL.