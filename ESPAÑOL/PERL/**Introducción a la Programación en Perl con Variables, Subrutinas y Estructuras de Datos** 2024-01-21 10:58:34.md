Aquí tienes un código complejo en PERL:

```
use strict;
use warnings;

my $cadena = "Hola mundo";
my @array = (1, 2, 3, 4, 5);
my %hash = (nombre => "Juan", edad => 20);

sub saludar {
    my $nombre = $_[0];
    print "Hola, $nombre!\n";
}

sub sumar {
    my $a = $_[0];
    my $b = $_[1];
    return $a + $b;
}

saludar($cadena);
my $resultado = sumar(3, 5);
print "El resultado de sumar 3 y 5 es $resultado\n";

foreach my $elemento (@array) {
    print "$elemento\n";
}

foreach my $clave (keys %hash) {
    my $valor = $hash{$clave};
    print "$clave: $valor\n";
}
```

Explicación del código:

* El código comienza importando los módulos `strict` y `warnings`, que ayudan a detectar errores en el código.
* Se declaran las variables `$cadena`, `@array` y `%hash`, que almacenan una cadena, una lista y un hash, respectivamente.
* Se definen las subrutinas `saludarl` y `sumar`, que sirven para saludar y sumar dos números, respectivamente.
* Se llama a la subrutina `saludarl` con el argumento `$cadena`, lo que imprime el mensaje "Hola, Hola mundo!".
* Se llama a la subrutina `sumar` con los argumentos 3 y 5, y el resultado se almacena en la variable `$resultado`.
* Se imprime el resultado de sumar 3 y 5.
* Se realiza un bucle que recorre la lista `@array` y imprime cada elemento de la lista.
* Se realiza un bucle que recorre las claves del hash `%hash` y, para cada clave, se imprime la clave y el valor asociado a la clave.