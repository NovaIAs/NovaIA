```perl
# Este código en PERL calcula el factorial de un número utilizando recursividad.

# Se define la función factorial que recibe un número y devuelve su factorial.
sub factorial {
    my $num = shift;
    return 1 if $num == 0;    # Caso base: el factorial de 0 es 1.
    return $num * factorial($num - 1);    # Caso recursivo: se multiplica el número por el factorial del número anterior.
}

# Se pide al usuario que introduzca un número.
print "Introduce un número: ";
my $numero = <STDIN>;
chomp $numero;    # Se elimina el salto de línea de la entrada del usuario.

# Se calcula el factorial del número introducido.
my $factorial = factorial($numero);

# Se muestra el resultado.
print "El factorial de $numero es $factorial.\n";
```

Explicación del código:

1. Se define la función `factorial` que recibe un número y devuelve su factorial. Esta función es recursiva, lo que significa que se llama a sí misma para resolver el problema.

2. El caso base de la recursión es cuando el número pasado a la función es 0. En este caso, la función devuelve 1, ya que el factorial de 0 es 1.

3. El caso recursivo es cuando el número pasado a la función es mayor que 0. En este caso, la función se llama a sí misma con el número menos 1, y multiplica el resultado por el número original. Esto se repite hasta que se llega al caso base.

4. Se pide al usuario que introduzca un número.

5. Se elimina el salto de línea de la entrada del usuario.

6. Se calcula el factorial del número introducido llamando a la función `factorial` con el número como argumento.

7. Se muestra el resultado.