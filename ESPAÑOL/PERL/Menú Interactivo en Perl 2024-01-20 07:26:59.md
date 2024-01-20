```perl
# Prueba de menú interactivo en Perl

use strict;
use warnings;

# Mostrar el menú
sub mostrar_menu {
  print "\n";
  print "Bienvenido al menú interactivo!\n";
  print "---------------------------------\n";
  print "1. Sumar dos números\n";
  print "2. Restar dos números\n";
  print "3. Multiplicar dos números\n";
  print "4. Dividir dos números\n";
  print "5. Salir\n";
  print "\n";
  print "Seleccione una opción (1-5): ";
}

# Obtener la opción seleccionada por el usuario
sub obtener_opcion {
  my $opcion = <STDIN>;
  chomp $opcion;
  return $opcion;
}

# Sumar dos números
sub sumar_numeros {
  print "Introduzca el primer número: ";
  my $num1 = <STDIN>;
  chomp $num1;

  print "Introduzca el segundo número: ";
  my $num2 = <STDIN>;
  chomp $num2;

  my $suma = $num1 + $num2;

  print "\nLa suma de los dos números es: $suma\n";
}

# Restar dos números
sub restar_numeros {
  print "Introduzca el primer número: ";
  my $num1 = <STDIN>;
  chomp $num1;

  print "Introduzca el segundo número: ";
  my $num2 = <STDIN>;
  chomp $num2;

  my $resta = $num1 - $num2;

  print "\nLa resta de los dos números es: $resta\n";
}

# Multiplicar dos números
sub multiplicar_numeros {
  print "Introduzca el primer número: ";
  my $num1 = <STDIN>;
  chomp $num1;

  print "Introduzca el segundo número: ";
  my $num2 = <STDIN>;
  chomp $num2;

  my $multiplicacion = $num1 * $num2;

  print "\nLa multiplicación de los dos números es: $multiplicacion\n";
}

# Dividir dos números
sub dividir_numeros {
  print "Introduzca el primer número: ";
  my $num1 = <STDIN>;
  chomp $num1;

  print "Introduzca el segundo número: ";
  my $num2 = <STDIN>;
  chomp $num2;

  if ($num2 == 0) {
    print "\n¡Error! No es posible dividir entre cero.\n";
  } else {
    my $division = $num1 / $num2;

    print "\nLa división de los dos números es: $division\n";
  }
}

# Salir del programa
sub salir {
  print "\n¡Gracias por usar el menú interactivo!\n";
  exit;
}

# Bucle principal del programa
my $continuar = 1;
while ($continuar) {
  mostrar_menu();

  my $opcion = obtener_opcion();

  if ($opcion == 1) {
    sumar_numeros();
  } elsif ($opcion == 2) {
    restar_numeros();
  } elsif ($opcion == 3) {
    multiplicar_numeros();
  } elsif ($opcion == 4) {
    dividir_numeros();
  } elsif ($opcion == 5) {
    salir();
  } else {
    print "\n¡Error! Opción no válida.\n";
  }

  print "\n¿Desea continuar (S/N)? ";
  my $respuesta = <STDIN>;
  chomp $respuesta;

  if ($respuesta eq "N" || $respuesta eq "n") {
    $continuar = 0;
  }
}
```

Explicación del código:

1. La línea `use strict;` activa el modo estricto de Perl, que ayuda a detectar errores comunes.

2. La línea `use warnings;` activa las advertencias de Perl, que ayudan a detectar posibles problemas en el código.

3. La subrutina `mostrar_menu()` muestra el menú interactivo al usuario.

4. La subrutina `obtener_opcion()` obtiene la opción seleccionada por el usuario.

5. Las subrutinas `sumar_numeros()`, `restar_numeros()`, `multiplicar_numeros()` y `dividir_numeros()` realizan las operaciones matemáticas correspondientes.

6. La subrutina `salir()` sale del programa.

7. En el bucle `while ($continuar)` se muestra el menú al usuario y se obtiene la opción seleccionada. Si la opción es válida, se ejecuta la subrutina correspondiente. Si la opción es 5, se llama a la subrutina `salir()` para salir del programa.

8. Al final del bucle, se pregunta al usuario si desea continuar. Si la respuesta es "N" o "n", se sale del bucle y el programa termina.