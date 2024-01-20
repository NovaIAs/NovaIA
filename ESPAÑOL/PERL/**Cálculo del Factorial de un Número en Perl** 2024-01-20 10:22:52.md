```perl
# Este código es un programa que calcula el factorial de un número.

# Definimos la función factorial que recibe un número entero y devuelve su factorial.
sub factorial {
  my $numero = shift;
  my $resultado = 1;  # El factorial de 0 es 1.
  for (my $i = 1; $i <= $numero; $i++) {
    $resultado *= $i;  # Multiplicamos el resultado por i en cada iteración.
  }
  return $resultado;
}

# Pedimos al usuario que introduzca un número entero.
print "Introduce un número entero: ";
my $numero = <STDIN>;  # Leemos la entrada del usuario.

# Calculamos el factorial del número.
my $factorial = factorial($numero);

# Mostramos el resultado al usuario.
print "El factorial de $numero es: $factorial\n";
```

Explicación del código:

* Definimos una función llamada `factorial` que recibe un número entero `$numero` como argumento y devuelve su factorial.
* Dentro de la función factorial, inicializamos una variable `$resultado` con el valor 1, ya que el factorial de 0 es 1.
* A continuación, utilizamos un bucle `for` para iterar sobre los números desde 1 hasta `$numero`.
* En cada iteración del bucle, multiplicamos `$resultado` por el valor de `$i`. Esto calcula el factorial del número actual.
* Después del bucle, devolvemos el valor de `$resultado`.
* En el programa principal, pedimos al usuario que introduzca un número entero.
* Calculamos el factorial del número utilizando la función `factorial`.
* Mostramos el resultado al usuario.