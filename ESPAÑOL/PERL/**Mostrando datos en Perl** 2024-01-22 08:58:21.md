```perl
use strict;
use warnings;

# Definir un hash para almacenar los datos
my %datos = (
    "nombre" => "Juan Pérez",
    "edad" => 25,
    "sexo" => "masculino",
    "ocupación" => "ingeniero de software",
    "hobbies" => ["leer", "escribir", "viajar"]
);

# Iterar sobre el hash y mostrar los datos
foreach my $clave (keys %datos) {
    my $valor = $datos{$clave};
    print "$clave: $valor\n";
}

# Crear un arreglo de números
my @numeros = (1, 2, 3, 4, 5);

# Iterar sobre el arreglo y mostrar los números
foreach my $numero (@numeros) {
    print "$numero\n";
}

# Definir una función para calcular el factorial de un número
sub factorial {
    my $numero = shift;
    my $resultado = 1;
    for (my $i = 1; $i <= $numero; $i++) {
        $resultado *= $i;
    }
    return $resultado;
}

# Mostrar el factorial de 5
print "El factorial de 5 es ", factorial(5), "\n";

# Crear una referencia a una variable
my $variable = "Hola mundo";
my $referencia = \$variable;

# Mostrar el valor de la variable usando la referencia
print "El valor de la variable es $referencia\n";

# Cambiar el valor de la variable usando la referencia
$$referencia = "Adiós mundo";

# Mostrar el nuevo valor de la variable
print "El nuevo valor de la variable es $variable\n";

# Crear un paquete
package MiPaquete;

# Definir una subrutina dentro del paquete
sub saludar {
    print "Hola mundo desde MiPaquete\n";
}

# Llamar a la subrutina desde fuera del paquete
MiPaquete::saludar();
```

Explicación del código:

* La primera parte del código define un hash llamado `%datos` para almacenar datos sobre una persona.
* La siguiente parte del código itera sobre el hash y muestra los datos en la consola.
* La siguiente parte del código crea un arreglo llamado `@numeros` y luego itera sobre el arreglo y muestra los números en la consola.
* La siguiente parte del código define una función llamada `factorial()` para calcular el factorial de un número.
* La siguiente parte del código muestra el factorial de 5.
* La siguiente parte del código crea una referencia a una variable llamada `$variable`.
* La siguiente parte del código muestra el valor de la variable usando la referencia.
* La siguiente parte del código cambia el valor de la variable usando la referencia.
* La siguiente parte del código muestra el nuevo valor de la variable.
* La siguiente parte del código crea un paquete llamado `MiPaquete`.
* La siguiente parte del código define una subrutina llamada `saludar()` dentro del paquete.
* La siguiente parte del código llama a la subrutina desde fuera del paquete.