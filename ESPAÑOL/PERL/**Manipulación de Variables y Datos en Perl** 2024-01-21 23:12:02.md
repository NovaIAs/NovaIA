```perl
# Importar los módulos necesarios
use strict;
use warnings;
use utf8;
use Encode;

# Declara variables
my $cadena = "Esta es una cadena de prueba";
my $expresion_regular = "/(\\w+)/";
my $numero = 10;
my %hash = ("nombre" => "Juan", "apellido" => "Pérez");

# Mostrar datos
print "Cadena: $cadena\n";
print "Expresión regular: $expresion_regular\n";
print "Número: $numero\n";
print "Hash: %hash\n";

# Manipular variables
$cadena = uc($cadena);
$expresion_regular =~ s/\\w+/mayúsculas/g;
$numero++;
%hash{"edad"} = 30;

# Mostrar datos modificados
print "\nCadena modificada: $cadena\n";
print "Expresión regular modificada: $expresion_regular\n";
print "Número modificado: $numero\n";
print "Hash modificado: %hash\n";

# Operaciones con variables
my $nueva_cadena = $cadena . " modificada";
my $resultado = $numero * 2;
my $valor = $hash{"nombre"};

# Mostrar resultados
print "\nNueva cadena: $nueva_cadena\n";
print "Resultado: $resultado\n";
print "Valor: $valor\n";

# Bucle for
for (my $i = 0; $i < $numero; $i++) {
  print "Número $i\n";
}

# Bucle foreach
foreach my $key (keys %hash) {
  print "Clave: $key, Valor: $hash{$key}\n";
}

# Condicional if/else
if ($numero > 10) {
  print "El número es mayor que 10\n";
} else {
  print "El número es menor o igual que 10\n";
}

# Expresión regular
my @matches = $cadena =~ /$expresion_regular/;
print "\nCoincidencias: @matches\n";
```

**Explicación:**

Este código Perl realiza varias tareas básicas y avanzadas, como la modificación de variables, la realización de operaciones, la iteración sobre datos, la toma de decisiones y el uso de expresiones regulares.

1. **Importar módulos:** Se importan los módulos necesarios para el código, como `strict`, `warnings` y `utf8`.

2. **Declarar variables:** Se declaran las variables que se utilizarán en el código, incluyendo una cadena, una expresión regular, un número y un hash.

3. **Mostrar datos:** Se muestran los valores de las variables en la consola.

4. **Manipular variables:** Se modifican los valores de las variables utilizando diferentes operadores y métodos.

5. **Mostrar datos modificados:** Se muestran los valores modificados de las variables en la consola.

6. **Operaciones con variables:** Se realizan operaciones con las variables, como concatenación de cadenas, multiplicación de números y obtención de valores de hash.

7. **Mostrar resultados:** Se muestran los resultados de las operaciones en la consola.

8. **Bucle for:** Se utiliza un bucle `for` para iterar sobre un rango de números.

9. **Bucle foreach:** Se utiliza un bucle `foreach` para iterar sobre las claves de un hash.

10. **Condicional if/else:** Se utiliza una expresión condicional `if/else` para tomar decisiones en función del valor de una variable.

11. **Expresión regular:** Se utiliza una expresión regular para buscar patrones en una cadena.

Este código es complejo y diferenciado porque combina varias tareas y utiliza diferentes características del lenguaje Perl. Es poco probable que se repita exactamente de la misma manera en otro código, ya que las variables, expresiones y operaciones específicas dependerán de los requisitos de cada programa.