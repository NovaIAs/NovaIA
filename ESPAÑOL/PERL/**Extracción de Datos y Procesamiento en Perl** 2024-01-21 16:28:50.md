```perl
use strict;
use warnings;

# Definir los datos de entrada
my $texto = "Tengo 27 años y soy de España";
my $patron = "(\d+)";

# Utilizar expresiones regulares para encontrar los números en el texto
my @numeros = $texto =~ /$patron/g;

# Recorrer los números encontrados y sumarlos
my $suma = 0;
foreach my $numero (@numeros) {
  $suma += $numero;
}

# Mostrar el resultado de la suma
print "La suma de los números encontrados en el texto es $suma\n";

# Definir una referencia a un array de colores
my $colores = ['rojo', 'naranja', 'amarillo', 'verde', 'azul', 'añil', 'violeta'];

# Obtener un color aleatorio del array
my $color = $colores->[int(rand($colores->@))];

# Mostrar el color aleatorio
print "El color aleatorio elegido es $color\n";

# Definir una estructura de datos hash para almacenar información sobre personas
my %personas = (
  'Juan' => {
    'edad' => 25,
    'pais' => 'México'
  },
  'María' => {
    'edad' => 30,
    'pais' => 'España'
  },
  'Pedro' => {
    'edad' => 35,
    'pais' => 'Argentina'
  }
);

# Obtener la información de una persona por su nombre
my $nombre = 'María';
my $persona = $personas{$nombre};

# Mostrar la información de la persona
print "Nombre: $persona->{nombre}\n";
print "Edad: $persona->{edad}\n";
print "País: $persona->{pais}\n";

# Definir una rutina para calcular el factorial de un número
sub factorial {
  my $numero = $_[0];
  if ($numero == 0) {
    return 1;
  } else {
    return $numero * factorial($numero - 1);
  }
}

# Obtener el factorial de un número
my $numero = 5;
my $factorial = factorial($numero);

# Mostrar el factorial del número
print "El factorial de $numero es $factorial\n";
```

Explicación del código:

* La primera línea utiliza la expresión regular `(\d+)` para encontrar todos los números en el texto. Los números se almacenan en el array `@numeros`.
* La siguiente línea recorre el array `@numeros` y suma los números encontrados. El resultado de la suma se almacena en la variable `$suma`.
* La línea siguiente imprime el resultado de la suma en la consola.
* La siguiente línea crea una referencia a un array de colores. El array contiene los colores "rojo", "naranja", "amarillo", "verde", "azul", "añil" y "violeta".
* La siguiente línea obtiene un color aleatorio del array. El color aleatorio se almacena en la variable `$color`.
* La línea siguiente imprime el color aleatorio en la consola.
* La siguiente línea define una estructura de datos hash para almacenar información sobre personas. El hash contiene información sobre tres personas: "Juan", "María" y "Pedro".
* La siguiente línea obtiene la información de una persona por su nombre. La información de la persona se almacena en la variable `$persona`.
* Las siguientes tres líneas imprimen la información de la persona en la consola.
* La siguiente línea define una rutina para calcular el factorial de un número. La rutina toma un número como argumento y devuelve el factorial del número.
* La siguiente línea obtiene el factorial de un número. El factorial del número se almacena en la variable `$factorial`.
* La línea siguiente imprime el factorial del número en la consola.